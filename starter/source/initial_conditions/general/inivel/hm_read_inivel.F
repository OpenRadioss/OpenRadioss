Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
C
Chd|====================================================================
Chd|  HM_READ_INIVEL                source/initial_conditions/general/inivel/hm_read_inivel.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        SUBROTVECT                    source/model/submodel/subrot.F
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INIVEL(V         , W      , ITAB     , ITABM1 , VR         ,
     .                          IGRNOD    , IGRBRIC, ISKN     , SKEW   , INIVIDS    ,
     .                          X         , UNITAB , LSUBMODEL, RTRANS , XFRAME     ,
     .                          IFRAME    , VFLOW  , WFLOW    , KXSP   , MULTI_FVM  ,
     .                          FVM_INIVEL, IGRQUAD, IGRSH3N  , RBY_MSN, RBY_INIAXIS)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C INITIAL VELOCITY READER (OPTIONS : /INIVEL/...)
C
C  /INIVEL/TRA       ITYPE=0
C  /INIVEL/ROT       ITYPE=1
C  /INIVEL/T+G       ITYPE=2
C  /INIVEL/GRID      ITYPE=3
C  /INIVEL/AXIS      ITYPE=4
C  /INIVEL/FVM       ITYPE=5
C  /INIVEL/NODE      ITYPE=6
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
      USE GROUPDEF_MOD
      USE HM_OPTION_READ_MOD
C----------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      INTEGER ITAB(*), ITABM1(*),ISKN(LISKN,*),
     .        INIVIDS(*),IFRAME(LISKN,*),KXSP(NISP,*),RBY_MSN(2,*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      my_real
     .   V(3,*),W(3,*),VR(3,*),SKEW(LSKEW,*),X(3,*),
     .   RTRANS(NTRANSF,*),XFRAME(NXFRAME,*),VFLOW(3,*) ,WFLOW(3,*),
     .   RBY_INIAXIS(7,*)
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
      TYPE(FVM_INIVEL_STRUCT), INTENT(INOUT) :: FVM_INIVEL(*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRQUAD) :: IGRQUAD
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,K,N,NRB,KPRI,KROT,NNOD,NOSYS,ITYPE,ID,ISK,IGR,IGRS,NBVEL
      INTEGER :: USER_UNIT_ID,NODID,NODID1,SUB_INDEX,IDIR
      INTEGER :: IDGRBRICK, IDGRQUAD, IDGRTRIA, IDGRBRICK_LOC, IDGRQUAD_LOC, IDGRTRIA_LOC
      INTEGER :: IAD1, IAD2,NODE,NL,JREC,NOD_COUNT,IAD,NODINIVEL,CPT,SUB_ID
      INTEGER :: FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP,IFRA,IFM,IUN,JJ,K1,K2,K3,INOD,NB_NODES, ID_NODE,IOK
      INTEGER :: NINIVEL_FVM,NINIVEL_TOTAL
      INTEGER :: FVM_GRBRIC_USER_ID(NINVEL), FVM_GRQUAD_USER_ID(NINVEL), FVM_GRTRIA_USER_ID(NINVEL) ! printout only
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGNO_RBY
      my_real :: V1, V2, V3, V4, V5, V6, VL1, VL2, VL3,VRA, OX, OY, OZ, NIXJ(6),VR1,VR2,VR3,VRL1,VRL2,VRL3,BID
      CHARACTER MESS*40,LLINE*ncharline,TITR*nchartitle,KEY*ncharkey,XYZ*ncharfield
      LOGICAL LV,LVR
      LOGICAL IS_AVAILABLE, IS_FOUND_UNIT_ID, IS_FOUND
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER,EXTERNAL :: USR2SYS
      DATA MESS/'INITIAL VELOCITIES DEFINITION           '/
      DATA IUN/1/
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_AVAILABLE = .FALSE.
      FLAG_FMT = 0
      NBVEL = 0
      ISK = 0
      IFRA = 0
      IFM = 0
      K1 = 0
      K2 = 0
      K3 = 0
      IDIR = 0
      KROT = 0
      NOD_COUNT = 0
      CALL HM_OPTION_COUNT('/INIVEL'    , NINIVEL_TOTAL)
      CALL HM_OPTION_COUNT('/INIVEL/FVM', NINIVEL_FVM)
C--------------------------------------------------
C LOOP OVER /INIVEL/... OPTIONS IN INPUT FILE
C--------------------------------------------------
      CALL HM_OPTION_START('/INIVEL')
      I = 0 

      DO CPT=1,HM_NINVEL
        I = I + 1
        !---SET CURSOR ON NEXT INIVEL OPTION
        CALL HM_OPTION_READ_KEY(LSUBMODEL,OPTION_ID = ID,UNIT_ID = USER_UNIT_ID,SUBMODEL_INDEX = SUB_INDEX,
     .                          SUBMODEL_ID = SUB_ID,OPTION_TITR = TITR,KEYWORD2 = KEY)

        !---CHECK EXISTING UNIT ID IF PROVIDED
        IS_FOUND_UNIT_ID = .FALSE.
        DO J=1,UNITAB%NUNITS
          IF (UNITAB%UNIT_ID(J) == USER_UNIT_ID) THEN
            IS_FOUND_UNIT_ID = .TRUE.
            EXIT
          ENDIF
        ENDDO
        IF (USER_UNIT_ID /= 0 .AND. .NOT.IS_FOUND_UNIT_ID) THEN
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                 I2=USER_UNIT_ID,I1=ID,C1='INITIAL VELOCITY',C2='INITIAL VELOCITY',C3=TITR) 
        ENDIF

        !---SET ITYPE DEPENDING ON USER KEYWORD
        FVM_INIVEL(I)%FLAG = .FALSE.
        IF(KEY(1:3)=='TRA')THEN
          ITYPE=0
        ELSEIF(KEY(1:3)=='ROT')THEN
          ITYPE=1
        ELSEIF(KEY(1:3)=='T+G')THEN
          ITYPE=2
        ELSEIF(KEY(1:3)=='GRI')THEN
          ITYPE=3
        ELSEIF(KEY(1:4)=='AXIS')THEN
          IF(INVERS < 120) THEN
            CALL ANCMSG(MSGID=2046,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='/INIVEL/AXIS',I1=INVERS)
          ENDIF
          ITYPE=4
        ELSEIF(KEY(1:3) == 'FVM') THEN
          ITYPE=5
          FVM_INIVEL(I)%FLAG = .TRUE.
        ELSEIF(KEY(1:4)=='NODE')THEN
          ITYPE=6
        ELSE
          CALL FREERR(1)
          RETURN
        ENDIF

        NBVEL = NBVEL+1
        INIVIDS(NBVEL)=ID

       IF(ITYPE > 6) THEN
          !invalid type
          CYCLE

        !---READER /INIVEL/TRA,ROT,T+G,GRID  (0,1,2,3)
        ELSEIF (ITYPE <= 3) THEN
          IFRA  = 0
          CALL HM_GET_INTV('entityid',IGR,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('inputsystem',ISK,IS_AVAILABLE,LSUBMODEL)
          IF(ISK == 0 .AND. SUB_INDEX /= 0 ) ISK = LSUBMODEL(SUB_INDEX)%SKEW
          CALL HM_GET_FLOATV('vector_X',VL1,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('vector_Y',VL2,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('vector_Z',VL3,IS_AVAILABLE,LSUBMODEL,UNITAB)

        !---READER /INIVEL/AXIS (4)
        ELSEIF (ITYPE == 4) THEN
          CALL HM_GET_STRING('rad_dir',XYZ,ncharfield,IS_AVAILABLE)
          CALL HM_GET_INTV('inputsystem',IFRA,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('entityid',IGR,IS_AVAILABLE,LSUBMODEL)

          CALL HM_GET_FLOATV('vector_X',VL1,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('vector_Y',VL2,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('vector_Z',VL3,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('rad_rotational_velocity',VRA,IS_AVAILABLE,LSUBMODEL,UNITAB)                                                 
          IF(IFRA == 0 .AND. SUB_INDEX  /=  0) CALL SUBROTVECT(VL1,VL2,VL3,RTRANS,SUB_ID,LSUBMODEL)
          IF(XYZ(1:1)=='X') THEN
            IDIR=1
          ELSEIF(XYZ(1:1)=='Y') THEN
            IDIR=2
          ELSEIF(XYZ(1:1)=='Z') THEN
            IDIR=3
          ELSE
            CALL ANCMSG(MSGID=933,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,C1=TITR)
          ENDIF
          ISK = 0

         !---READER /INIVEL/FVM (5)
        ELSEIF (ITYPE == 5) THEN
          CALL HM_GET_FLOATV('Vx', VL1, IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('Vy', VL2, IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('Vz', VL3, IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_INTV('grbric_ID', IDGRBRICK, IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_INTV('grqd_ID', IDGRQUAD, IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_INTV('grtria_ID', IDGRTRIA, IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_INTV('skew_ID', ISK, IS_AVAILABLE, LSUBMODEL)

         !---READER /INIVEL/NODE (6)
        ELSEIF (ITYPE == 6) THEN
          CALL HM_GET_INTV('NB_NODES', NB_NODES, IS_AVAILABLE, LSUBMODEL)
          DO N=1,NB_NODES
            CALL HM_GET_INT_ARRAY_INDEX('NODE', ID_NODE, N, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('SKEWA', ISK, N, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_FLOAT_ARRAY_INDEX('VXTA', VL1, N, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('VYTA', VL2, N, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('VZTA', VL3, N, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('VXRA', VR1, N, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('VYRA', VR2, N, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('VZRA', VR3, N, IS_AVAILABLE, LSUBMODEL, UNITAB)
            IOK = 0
            KROT = 1
            IF (ID_NODE > 0) THEN
              IF (ISK > 0) THEN
                DO J=0,NUMSKW+MIN(IUN,NSPCOND)*NUMSPH+NSUBMOD
                  IF (ISK == ISKN(4,J+1)) THEN
                    ISK=J+1
                    V1 = SKEW(1,ISK)*VL1+SKEW(4,ISK)*VL2+SKEW(7,ISK)*VL3
                    V2 = SKEW(2,ISK)*VL1+SKEW(5,ISK)*VL2+SKEW(8,ISK)*VL3
                    V3 = SKEW(3,ISK)*VL1+SKEW(6,ISK)*VL2+SKEW(9,ISK)*VL3
                    V4 = SKEW(1,ISK)*VR1+SKEW(4,ISK)*VR2+SKEW(7,ISK)*VR3
                    V5 = SKEW(2,ISK)*VR1+SKEW(5,ISK)*VR2+SKEW(8,ISK)*VR3
                    V6 = SKEW(3,ISK)*VR1+SKEW(6,ISK)*VR2+SKEW(9,ISK)*VR3
                    IOK = 1
                  ENDIF
                ENDDO
                IF (IOK == 0)CALL ANCMSG(MSGID=184,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .                                   I1=ID,I2=ISK,C1='INITIAL VELOCITY',C2='INITIAL VELOCITY',C3=TITR)
                 NOSYS = USR2SYS(ID_NODE,ITABM1,MESS,ID)
                 V(1,NOSYS)  = V1
                 V(2,NOSYS)  = V2
                 V(3,NOSYS)  = V3
                 VR(1,NOSYS) = V4
                 VR(2,NOSYS) = V5
                 VR(3,NOSYS) = V6
              ELSEIF (ISK == 0 .AND. IFRA == 0) THEN
                 NOSYS = USR2SYS(ID_NODE,ITABM1,MESS,ID)
                 V(1,NOSYS)  = VL1
                 V(2,NOSYS)  = VL2
                 V(3,NOSYS)  = VL3
                 VR(1,NOSYS) = VR1
                 VR(2,NOSYS) = VR2
                 VR(3,NOSYS) = VR3
              ENDIF
            ENDIF
          ENDDO !N=1,NB_NODES
          ISK = 0

        ENDIF !ITYPE TEST
C
        IF (ITYPE /= 6) THEN
          IF (ISK > 0) THEN
              IS_FOUND = .FALSE.
              DO J=0,NUMSKW+MIN(IUN,NSPCOND)*NUMSPH+NSUBMOD
                IF (ISK == ISKN(4,J+1)) THEN
                  ISK=J+1
                  V1 = SKEW(1,ISK)*VL1+SKEW(4,ISK)*VL2+SKEW(7,ISK)*VL3
                  V2 = SKEW(2,ISK)*VL1+SKEW(5,ISK)*VL2+SKEW(8,ISK)*VL3
                  V3 = SKEW(3,ISK)*VL1+SKEW(6,ISK)*VL2+SKEW(9,ISK)*VL3
                  IS_FOUND = .TRUE.
                  EXIT
                ENDIF
              ENDDO
              IF(.NOT. IS_FOUND)THEN
                CALL ANCMSG(MSGID=184,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,I2=ISK,
     .                      C1='INITIAL VELOCITY', C2='INITIAL VELOCITY', C3=TITR)
              ENDIF

          ELSEIF (IFRA > 0) THEN
            IS_FOUND = .FALSE.
            DO K=1,NUMFRAM
              J=K+1
              IF(IFRA==IFRAME(4,J)) THEN
                V1 = XFRAME(1,J)*VL1+XFRAME(4,J)*VL2+XFRAME(7,J)*VL3
                V2 = XFRAME(2,J)*VL1+XFRAME(5,J)*VL2+XFRAME(8,J)*VL3
                V3 = XFRAME(3,J)*VL1+XFRAME(6,J)*VL2+XFRAME(9,J)*VL3
                IS_FOUND = .TRUE.
                EXIT
              ENDIF
            ENDDO
            IF(.NOT. IS_FOUND)THEN
              CALL ANCMSG(MSGID=490,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,I2=IFRA,
     .                    C1='INITIAL VELOCITY',C2='INITIAL VELOCITY',C3=TITR)
            ENDIF
            IFM = J
          ELSEIF (ISK == 0 .AND. IFRA == 0) THEN
            V1 = VL1
            V2 = VL2
            V3 = VL3
          ENDIF 
        ENDIF

        IF (ITYPE == 5) THEN
           IF (.NOT. MULTI_FVM%IS_USED) THEN
              CALL ANCMSG(MSGID=1554,MSGTYPE=MSGERROR,ANMODE=ANINFO,C1='IN /INIVEL OPTION')
           ELSE
              IDGRBRICK_LOC = -1
              IDGRQUAD_LOC  = -1
              IDGRTRIA_LOC  = -1
              IF (IDGRBRICK + IDGRQUAD + IDGRTRIA == 0) THEN
                 CALL ANCMSG(MSGID=1553, MSGTYPE=MSGWARNING, ANMODE=ANINFO,C1='IN /INIVEL OPTION')
              ELSE
                 IF (IDGRBRICK /= 0) THEN
                    DO J = 1,NGRBRIC
                       IF (IDGRBRICK == IGRBRIC(J)%ID) IDGRBRICK_LOC = J
                    ENDDO
                    IF (IDGRBRICK_LOC == -1) THEN
                       CALL ANCMSG(MSGID=1554, MSGTYPE=MSGERROR,ANMODE=ANINFO,C1='IN /INIVEL OPTION',I1=IDGRBRICK)
                    ENDIF
                 ENDIF
                 IF (IDGRQUAD /= 0) THEN
                    DO J = 1,NGRQUAD
                       IF (IDGRQUAD == IGRQUAD(J)%ID) IDGRQUAD_LOC = J
                    ENDDO    
                    IF (IDGRQUAD_LOC == -1) THEN
                       CALL ANCMSG(MSGID=1554,MSGTYPE=MSGERROR,ANMODE=ANINFO,C1='IN /INIVEL OPTION',I1=IDGRQUAD)
                    ENDIF
                 ENDIF
                 IF (IDGRTRIA /= 0) THEN
                    DO J = 1,NGRSH3N
                       IF (IDGRTRIA == IGRSH3N(J)%ID) IDGRTRIA_LOC = J
                    ENDDO      
                    IF (IDGRTRIA_LOC == -1) THEN
                       CALL ANCMSG(MSGID=1554,MSGTYPE=MSGERROR,ANMODE=ANINFO,C1='IN /INIVEL OPTION',I1=IDGRTRIA)
                    ENDIF
                 ENDIF
              ENDIF
              !     Going on
              !     Brick groups
              FVM_INIVEL(I)%FLAG = .TRUE.
              FVM_INIVEL(I)%GRBRICID = IDGRBRICK_LOC
              FVM_INIVEL(I)%GRQUADID = IDGRQUAD_LOC
              FVM_INIVEL(I)%GRSH3NID = IDGRTRIA_LOC
              FVM_INIVEL(I)%VX = V1
              FVM_INIVEL(I)%VY = V2
              FVM_INIVEL(I)%VZ = V3
              FVM_GRBRIC_USER_ID(I) = IDGRBRICK
              FVM_GRQUAD_USER_ID(I) = IDGRQUAD
              FVM_GRTRIA_USER_ID(I) = IDGRTRIA
           ENDIF 
        ENDIF 
C
        IF (ITYPE /= 5 .AND. ITYPE /= 6) THEN
           IGRS=0
           IF (IGR == 0) THEN
              CALL ANCMSG(MSGID=668,MSGTYPE=MSGERROR,ANMODE=ANINFO,C1='/INIVEL',C2='/INIVEL',C3=TITR,I1=ID)
           ENDIF
           DO J=1,NGRNOD
              IF(IGR == IGRNOD(J)%ID) IGRS=J
           ENDDO
           IF(IGRS /= 0)THEN
              DO J=1,IGRNOD(IGRS)%NENTITY
                 NOSYS=IGRNOD(IGRS)%ENTITY(J)
                 IF(ITYPE == 0) THEN
                    V(1,NOSYS)=V1
                    V(2,NOSYS)=V2
                    V(3,NOSYS)=V3
                    IF(IALELAG > 0) THEN
                       VFLOW(1,NOSYS) = V1
                       VFLOW(2,NOSYS) = V2
                       VFLOW(3,NOSYS) = V3
                       WFLOW(1,NOSYS) = V1
                       WFLOW(2,NOSYS) = V2
                       WFLOW(3,NOSYS) = V3
                    ENDIF
                 ELSEIF(ITYPE == 1) THEN
                    KROT = 1
                    IF (IRODDL>0) THEN
                      VR(1,NOSYS)=V1
                      VR(2,NOSYS)=V2
                      VR(3,NOSYS)=V3
                    ENDIF
                 ELSEIF(ITYPE == 2) THEN
                    V(1,NOSYS)=V1
                    V(2,NOSYS)=V2
                    V(3,NOSYS)=V3
                    IF (IALE == 1) THEN
                       W(1,NOSYS)=V1
                       W(2,NOSYS)=V2
                       W(3,NOSYS)=V3
                    ENDIF
                    IF(IALELAG > 0) THEN
                       VFLOW(1,NOSYS) = V1
                       VFLOW(2,NOSYS) = V2
                       VFLOW(3,NOSYS) = V3
                       WFLOW(1,NOSYS) = V1
                       WFLOW(2,NOSYS) = V2
                       WFLOW(3,NOSYS) = V3
                    ENDIF
                 ELSEIF(ITYPE == 3) THEN
                    W(1,NOSYS)=V1
                    W(2,NOSYS)=V2
                    W(3,NOSYS)=V3
                    IF(IALELAG > 0) THEN
                       VFLOW(1,NOSYS) = V1
                       VFLOW(2,NOSYS) = V2
                       VFLOW(3,NOSYS) = V3
                       WFLOW(1,NOSYS) = V1
                       WFLOW(2,NOSYS) = V2
                       WFLOW(3,NOSYS) = V3
                    ENDIF
                 ELSEIF(ITYPE == 4) THEN
C--                 /INIVEL/AXIS -> tag of main nodes of rbody
                    IF ((.NOT.ALLOCATED(TAGNO_RBY)).AND.(NRBODY > 0)) THEN
                      ALLOCATE(TAGNO_RBY(NUMNOD))
                      TAGNO_RBY(1:NUMNOD) = 0
                      DO NRB=1,NRBODY
                        TAGNO_RBY(RBY_MSN(2,NRB)) = NRB
                      ENDDO
                    ENDIF
                    NIXJ = ZERO
                    IF (IFRA > 0) THEN
                       K1=3*IDIR-2
                       K2=3*IDIR-1
                       K3=3*IDIR
                       OX  = XFRAME(10,IFM)
                       OY  = XFRAME(11,IFM)
                       OZ  = XFRAME(12,IFM)
                       NIXJ(1)=XFRAME(K1,IFM)*(X(2,NOSYS)-OY)
                       NIXJ(2)=XFRAME(K2,IFM)*(X(1,NOSYS)-OX)
                       NIXJ(3)=XFRAME(K2,IFM)*(X(3,NOSYS)-OZ)
                       NIXJ(4)=XFRAME(K3,IFM)*(X(2,NOSYS)-OY)
                       NIXJ(5)=XFRAME(K3,IFM)*(X(1,NOSYS)-OX)
                       NIXJ(6)=XFRAME(K1,IFM)*(X(3,NOSYS)-OZ)
                       IF (IRODDL>0) THEN
                          VR(1,NOSYS)= VRA*XFRAME(K1,IFM)
                          VR(2,NOSYS)= VRA*XFRAME(K2,IFM)
                          VR(3,NOSYS)= VRA*XFRAME(K3,IFM)
                       END IF
                    ELSE
                       IF(IDIR==1) THEN
                          NIXJ(1)=X(2,NOSYS)
                          NIXJ(6)=X(3,NOSYS)
                       ELSEIF(IDIR==2) THEN
                          NIXJ(2)=X(1,NOSYS)
                          NIXJ(3)=X(3,NOSYS)
                       ELSEIF(IDIR==3) THEN
                          NIXJ(4)=X(2,NOSYS)
                          NIXJ(5)=X(1,NOSYS)
                       ENDIF
                       IF (IRODDL>0) THEN
                          VR(1,NOSYS)= ZERO !VRA*XFRAME(K1,IFM)
                          VR(2,NOSYS)= ZERO !VRA*XFRAME(K2,IFM)
                          VR(3,NOSYS)= ZERO !VRA*XFRAME(K3,IFM)
                          IF (IDIR==1) VR(1,NOSYS)= VRA
                          IF (IDIR==2) VR(2,NOSYS)= VRA
                          IF (IDIR==3) VR(3,NOSYS)= VRA
                       END IF
                    ENDIF
                    V(1,NOSYS)= V1+VRA*(NIXJ(3)-NIXJ(4))
                    V(2,NOSYS)= V2+VRA*(NIXJ(5)-NIXJ(6))
                    V(3,NOSYS)= V3+VRA*(NIXJ(1)-NIXJ(2))
                    IF(IALELAG > 0) THEN
                       VFLOW(1,NOSYS) = V(1,NOSYS)
                       VFLOW(2,NOSYS) = V(2,NOSYS)
                       VFLOW(3,NOSYS) = V(3,NOSYS)
                       WFLOW(1,NOSYS) = V(1,NOSYS)
                       WFLOW(2,NOSYS) = V(2,NOSYS)
                       WFLOW(3,NOSYS) = V(3,NOSYS)
                    ENDIF

C--                 /INIVEL/AXIS -> data must be stored to update initial velocity when RBODY main node is moved (inirby.F)
                    IF (NRBODY > 0) THEN
                      IF (TAGNO_RBY(NOSYS) > 0) THEN
                        RBY_INIAXIS(1,TAGNO_RBY(NOSYS)) = ONE
                        RBY_INIAXIS(2,TAGNO_RBY(NOSYS)) = V(1,NOSYS)
                        RBY_INIAXIS(3,TAGNO_RBY(NOSYS)) = V(2,NOSYS)
                        RBY_INIAXIS(4,TAGNO_RBY(NOSYS)) = V(3,NOSYS)
                        IF (IRODDL>0) THEN
                          RBY_INIAXIS(5,TAGNO_RBY(NOSYS)) = VR(1,NOSYS)
                          RBY_INIAXIS(6,TAGNO_RBY(NOSYS)) = VR(2,NOSYS)
                          RBY_INIAXIS(7,TAGNO_RBY(NOSYS)) = VR(3,NOSYS)
                        ENDIF                   
                      ENDIF
                    ENDIF
                 ENDIF
              ENDDO
              NNOD=IGRNOD(IGRS)%NENTITY
           ELSE
              CALL ANCMSG(MSGID=53,MSGTYPE=MSGERROR,ANMODE=ANINFO,C1='IN /INIVEL OPTION',I1=IGR)
           ENDIF
        ENDIF ! IF (ITYPE /= 5 .AND. ITYPE /= 6)
      ENDDO

      IF (ALLOCATED(TAGNO_RBY)) DEALLOCATE(TAGNO_RBY)

      CALL UDOUBLE(INIVIDS,1,NBVEL,MESS,0,BID)

      !--- Reset velocities for the dormant SPH particles in the reservoir
      IF (NSPHRES>0) THEN
        DO N=1,NSPHRES
          INOD = KXSP(3,FIRST_SPHRES+N-1)
          V(1,INOD) = ZERO
          V(2,INOD) = ZERO
          V(3,INOD) = ZERO
          IF (IRODDL>0) THEN
            VR(1,INOD) = ZERO
            VR(2,INOD) = ZERO
            VR(3,INOD) = ZERO
          ENDIF
        END DO   
      ENDIF


      !--------------------------------------------------
      !     STARTER LISTING FILE (INITIAL VELOCITY PRINTED IF IPRI >= 2)
      !            IPRI : SEE /IOFLAG OPTION
      !--------------------------------------------------
      IF (HM_NINVEL > 0) THEN
        J=0
        NODINIVEL=0

        ! INITIAL VELOCIIES FOR STAGGERED SCHEME
        IF(IPRI >= 2 .AND. NINIVEL_TOTAL-NINIVEL_FVM > 0 )THEN

          !---TITLE OUTPUT
          !     STAGGERED SCHEME (VELOCITIES AT NODES)
          IF(IALE /= 0) THEN
            WRITE(IOUT,2100)
          ELSEIF(KROT == 0) THEN
            WRITE(IOUT,2000)
          ELSE
            WRITE(IOUT,2200)
          ENDIF

          !---DETAILS OUTPUT--------------------------
          !     STAGGERED SCHEME (VELOCITIES AT NODES)
            KPRI=0
            DO N=1,NUMNOD,50
              J=J+50
              J=MIN(J,NUMNOD)
              IF(IALE == 0)THEN
                DO I=N,J
                  IF(KPRI >= 50) THEN
                    IF(KROT == 0) THEN
                      WRITE(IOUT,2000)
                    ELSE
                      WRITE(IOUT,2200)
                    ENDIF
                    KPRI=0
                  ENDIF
                  IF(IRODDL /= 0) THEN
                    IF (V(1,I)/=ZERO.OR.V(2,I)/=ZERO.OR.V(3,I)/=ZERO.OR.VR(1,I)/=ZERO.OR.VR(2,I)/=ZERO.OR.VR(3,I)/=ZERO)THEN
                      NODINIVEL=NODINIVEL+1
                      IF (VR(1,I) /= ZERO .OR. VR(2,I) /= ZERO .OR. VR(3,I) /= ZERO) THEN
                        WRITE(IOUT,'(3X,I10,8X,1P6G20.13)') ITAB(I),V(1,I),V(2,I),V(3,I),VR(1,I),VR(2,I),VR(3,I)
                      ELSE
                        WRITE(IOUT,'(3X,I10,8X,1P6G20.13)')ITAB(I),V(1,I),V(2,I),V(3,I)
                      ENDIF
                      KPRI=KPRI+1
                    ENDIF
                  ELSEIF(V(1,I) /= ZERO .OR. V(2,I) /= ZERO .OR. V(3,I) /= ZERO) THEN
                    NODINIVEL=NODINIVEL+1
                    WRITE(IOUT,'(3X,I10,8X,1P6G20.13)')ITAB(I),V(1,I),V(2,I),V(3,I)
                    KPRI=KPRI+1
                  ENDIF
                ENDDO!next I

              ELSEIF(IALE /= 0)THEN
                DO I=N,J
                  IF(KPRI==50) THEN
                    WRITE(IOUT,2100)
                    KPRI=0
                  ENDIF
                  IF(V(1,I)/=ZERO.OR.V(2,I)/=ZERO.OR.V(3,I)/=ZERO.OR.W(1,I)/=ZERO.OR.W(2,I)/=ZERO.OR.W(3,I)/=ZERO) THEN
                    NODINIVEL=NODINIVEL+1
                    WRITE(IOUT,'(5X,I10,8X,1P6G20.13)') ITAB(I),V(1,I),V(2,I),V(3,I),W(1,I),W(2,I),W(3,I)
                    KPRI=KPRI+1
                  ENDIF
                ENDDO! NEXT I
              ENDIF

            ENDDO!NEXT N
            WRITE(IOUT,'(/,A,I10,//)') ' NUMBER OF NODES WITH INITIAL VELOCITY:',NODINIVEL

        ENDIF

        ! INITIAL VELOCIIES FOR COLLOCATED SCHEME
        IF(IPRI >= 2 .AND. NINIVEL_FVM > 0 )THEN
          WRITE(IOUT,3000)
          !---DETAILS OUTPUT-----------------------------------
          !     COLOCATED SCHEME (VELOCITIES AT CELL CENTROIDS)
            DO CPT=1,HM_NINVEL
              IF(.NOT. FVM_INIVEL(I)%FLAG)CYCLE
              V1=FVM_INIVEL(I)%VX
              V2=FVM_INIVEL(I)%VY
              V3=FVM_INIVEL(I)%VZ
              IF(IDGRBRICK_LOC >0)THEN
                WRITE(IOUT,3001)
                WRITE(IOUT,'(5X,I10,8X,1P6G20.13)') FVM_GRBRIC_USER_ID(I),V1,V2,V3
              ENDIF
              IF(IDGRQUAD_LOC >0)THEN
                WRITE(IOUT,3002)
                WRITE(IOUT,'(5X,I10,8X,1P6G20.13)') FVM_GRQUAD_USER_ID(I),V1,V2,V3
              ENDIF
              IF(IDGRTRIA_LOC >0)THEN
                WRITE(IOUT,3003)
                WRITE(IOUT,'(5X,I10,8X,1P6G20.13)') FVM_GRTRIA_USER_ID(I),V1,V2,V3
              ENDIF
            ENDDO!next CPT
             WRITE(IOUT,'(//)')
        ENDIF!IF(IPRI >= 2)

      ENDIF!(HM_NINVEL > 0)
!-----------
      RETURN
!-----------
2000  FORMAT(//
     .'     INITIAL VELOCITIES    '/
     .'     -------------------   '/
     + 9X,'NODE',22X,'VX   ',15X,'VY   ',15X,'VZ   '/)
2100  FORMAT(//
     .'     INITIAL VELOCITIES    '/
     .'     -------------------   '/
     + 9X,'NODE',22X,'VX   ',15X,'VY   ',15X,'VZ   ',
     +           14X,'WX   ',15X,'WY   ',15X,'WZ   '/)
2200  FORMAT(//
     .'     INITIAL VELOCITIES    '/
     .'     -------------------   '/
     + 9X,'NODE',22X,'VX   ',15X,'VY   ',15X,'VZ   ',
     +           14X,'VRX  ',15X,'VRY  ',15X,'VRZ'/)
3000  FORMAT(//
     .'     INITIAL VELOCITIES (FVM)   '/
     .'     ------------------------   ')
3001  FORMAT(
     + 9X,'GRBRIC',22X,'VX   ',15X,'VY   ',15X,'VZ   ')
3002  FORMAT(
     + 9X,'GRQUAD',22X,'VX   ',15X,'VY   ',15X,'VZ   ')
3003  FORMAT(
     + 9X,'GRTRIA',22X,'VX   ',15X,'VY   ',15X,'VZ   ')

!-----------
      END SUBROUTINE HM_READ_INIVEL


