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
Chd|  HM_READ_SLIPRING              source/tools/seatbelts/hm_read_slipring.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        MYQSORT                       ../common_source/tools/sort/myqsort.F
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        NINTRI                        source/system/nintrr.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_SLIPRING(LSUBMODEL,ITABM1,IXR,ITAB,UNITAB,
     .                            X,FUNC_ID,NOM_OPT,ALEA,IGRNOD,
     .                            IGRSH4N,IXC,IPM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE UNITAB_MOD
      USE SEATBELT_MOD
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
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "submod_c.inc"
#include      "random_c.inc"
#include      "tabsiz_c.inc"
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) ::  ITABM1(NUMNOD),IXR(NIXR,NUMELR),IXC(NIXC,NUMELC),ITAB(NUMNOD),FUNC_ID(NFUNCT),IPM(NPROPMI,NUMMAT)
      INTEGER, INTENT(INOUT) :: NOM_OPT(LNOPT1,SNOM_OPT1)
      my_real, INTENT(IN) ::  ALEA(NRAND)
      my_real, INTENT(INOUT) ::  X(3,NUMNOD)
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRSHEL) :: IGRSH4N
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ,DIMENSION(NSLIPRING) :: SLIP_ID
      INTEGER :: I,J,K,L,ID, UID, NODE_ID, EL1, EL2, IERR1, NODE_ID2
      INTEGER :: NODE1,NODE2,NODE3,NODE4,EL1_LOC,EL2_LOC,BID,ISENS,FLOW_FLAG
      INTEGER :: IFUNC(4),IFUNC_LOC(4),NFRAM,ISHELL,GR_NOD,GR_SHEL1,GR_SHEL2,GRN_LOC,GRS1_LOC,GRS2_LOC
      INTEGER :: N_FIRST,N_LAST,NJ,NODE,IPOS,IERROR,NJ_NEXT,MID,MTYP
      INTEGER , DIMENSION(:), ALLOCATABLE:: TAGNO,ELEM1_NOD,ELEM2_NOD,CORES1,CORES2,IPOS1_NOD,IPOS2_NOD,JPERM
      my_real :: DISTN,DIST1,DIST2,DIST3,A,ED_FACTOR,FRICD,XSCALE1,YSCALE2,XSCALE2,FRICS,XSCALE3,YSCALE4,XSCALE4
      my_real :: XSCALE1_UNIT,XSCALE2_UNIT,NN(3),NORM,N1(3),N2(3),N3(3),SCAL,ALEA_MAX,TOLE_2,NORMJ
      my_real :: DIST_MIN,VECT(3),VECTJ(3)
      my_real , DIMENSION(:), ALLOCATABLE:: DIST
      CHARACTER             :: TITR*nchartitle,KEY*ncharkey,MESS*40
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
!
      LOGICAL :: IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS,NINTRI,NGR2USR
C=======================================================================
      DATA MESS/'SLIPRING DEFINITION                     '/
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IERR1      = 0
C
      IF(NSLIPRING > 0 ) THEN
C
        WRITE(IOUT,1000)
C
        MY_ALLOCATE (SLIPRING,NSLIPRING)
        DO I=1,NSLIPRING
          SLIPRING(I)%ID = 0
          SLIPRING(I)%IDG = 0
          SLIPRING(I)%NFRAM = 0
          SLIPRING(I)%IFUNC = 0
          SLIPRING(I)%SENSID = 0
          SLIPRING(I)%FL_FLAG = 0
          SLIPRING(I)%RBODY = 0
          SLIPRING(I)%A = ZERO
          SLIPRING(I)%DC = ZERO
          SLIPRING(I)%FRIC = ZERO
          SLIPRING(I)%FAC_D = ZERO
          SLIPRING(I)%FRICS = ZERO
          SLIPRING(I)%FAC_S = ZERO
        ENDDO  
C
        CALL HM_OPTION_START('/SLIPRING')
C
        DO I = 1,NSLIPRING
C
          CALL HM_OPTION_READ_KEY(LSUBMODEL,OPTION_TITR=TITR,OPTION_ID=ID,UNIT_ID=UID,KEYWORD2=KEY)
C
          NOM_OPT(1,I)=ID
          CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,I),LTITR)
C
          CALL HM_GET_INTV('Sens_ID', ISENS, IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_INTV('Flow_flag', FLOW_FLAG, IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_FLOATV('A',A,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Ed_factor',ED_FACTOR,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
          CALL HM_GET_INTV('Fct_ID1', IFUNC(1), IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_INTV('Fct_ID2', IFUNC(2), IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_FLOATV('Fricd',FRICD,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Xscale1',XSCALE1,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Yscale2',YSCALE2,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Xscale2',XSCALE2,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
          CALL HM_GET_INTV('Fct_ID3', IFUNC(3), IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_INTV('Fct_ID4', IFUNC(4), IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_FLOATV('Frics',FRICS,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Xscale3',XSCALE3,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Yscale4',YSCALE4,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Xscale4',XSCALE4,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
          CALL HM_GET_FLOATV_DIM('Xscale1',XSCALE1_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV_DIM('Xscale2',XSCALE2_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
          SLIP_ID(I) = ID
C
          IF (IFUNC(1) > 0) THEN
            IF (FRICD== ZERO) FRICD = ONE
            IF (XSCALE1== ZERO) XSCALE1 = ONE*XSCALE1_UNIT    
          ENDIF
C
          IF (IFUNC(2) > 0) THEN
            IF (YSCALE2== ZERO) YSCALE2 = ONE
            IF (XSCALE2== ZERO) XSCALE2 = ONE*XSCALE2_UNIT    
          ENDIF
C
          IF (IFUNC(3) > 0) THEN
            IF (FRICS== ZERO) FRICS = ONE
            IF (XSCALE3== ZERO) XSCALE3 = ONE*XSCALE1_UNIT    
          ENDIF
C
          IF (IFUNC(4) > 0) THEN
            IF (YSCALE4== ZERO) YSCALE4 = ONE
            IF (XSCALE4== ZERO) XSCALE4 = ONE*XSCALE2_UNIT  
          ENDIF
C
C---------Check of sensors is done in creat_seatblet as sensors are not yet available
C
C---------Check of functions
C
          IFUNC_LOC(1:4) = 0
C              
          DO J=1,4       
            IF (IFUNC(J) > 0) THEN
              DO K=1,NFUNCT               
                IF (FUNC_ID(K) == IFUNC(J)) IFUNC_LOC(J) = K                                        
              ENDDO         
              IF(IFUNC_LOC(J) == 0) CALL ANCMSG(MSGID=2002,
     .                                   MSGTYPE=MSGERROR,
     .                                   ANMODE=ANINFO_BLIND_1,
     .                                   C1='FUNCTION',
     .                                   I1=ID,I2=IFUNC(J))
            ENDIF
          ENDDO
C
          SLIPRING(I)%ID = ID
          SLIPRING(I)%SENSID = ISENS
          SLIPRING(I)%FL_FLAG = FLOW_FLAG
C
          SLIPRING(I)%IFUNC(1) = IFUNC_LOC(1) 
          SLIPRING(I)%IFUNC(2) = IFUNC_LOC(2) 
          SLIPRING(I)%IFUNC(3) = IFUNC_LOC(3) 
          SLIPRING(I)%IFUNC(4) = IFUNC_LOC(4)
C
          SLIPRING(I)%DC = ED_FACTOR
          SLIPRING(I)%A = A
C
          SLIPRING(I)%FRIC = FRICD
          SLIPRING(I)%FAC_D(1) = XSCALE1
          SLIPRING(I)%FAC_D(2) = XSCALE2
          SLIPRING(I)%FAC_D(3) = YSCALE2
          SLIPRING(I)%FRICS = FRICS
          SLIPRING(I)%FAC_S(1) = XSCALE3
          SLIPRING(I)%FAC_S(2) = XSCALE4
          SLIPRING(I)%FAC_S(3) = YSCALE4    
C
          IF (KEY(1:6)=='SPRING') THEN
C
C---------- SLIPRING/SPRING------------------------------------------------------------------
C
            CALL HM_GET_INTV('EL_ID1', EL1, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INTV('EL_ID2', EL2, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INTV('Node_ID', NODE_ID, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INTV('Node_ID2', NODE_ID2, IS_AVAILABLE, LSUBMODEL)
C
            WRITE(IOUT,1100) ID,TRIM(TITR),EL1,EL2,NODE_ID,NODE_ID2,ISENS,FLOW_FLAG,A,ED_FACTOR,
     .                     IFUNC(1),IFUNC(2),FRICD,XSCALE1,YSCALE2,XSCALE2,
     .                     IFUNC(3),IFUNC(4),FRICS,XSCALE3,YSCALE4,XSCALE4
C
C
            EL1_LOC=NINTRI(EL1,IXR,NIXR,NUMELR,NIXR)
            EL2_LOC=NINTRI(EL2,IXR,NIXR,NUMELR,NIXR)
C
            IF(EL1_LOC == 0) THEN
              CALL ANCMSG(MSGID=2002,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    C1='SPRING ELEMENT',I1=ID,I2=EL1)
            ELSE
              MTYP = 0
              MID = IXR(5,EL1_LOC)
              IF (MID > 0) MTYP = IPM(2,MID)
              IF (MTYP /= 114) CALL ANCMSG(MSGID=2032,
     .                                     MSGTYPE=MSGERROR,
     .                                     ANMODE=ANINFO,
     .                                     I1=ID,I2=EL1) 
            ENDIF
C
            IF(EL2_LOC == 0) THEN
              CALL ANCMSG(MSGID=2002,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    C1='SPRING ELEMENT',I1=ID,I2=EL2)
            ELSE
              MTYP = 0
              MID = IXR(5,EL1_LOC)
              IF (MID > 0) MTYP = IPM(2,MID)
              IF (MTYP /= 114) CALL ANCMSG(MSGID=2032,
     .                                     MSGTYPE=MSGERROR,
     .                                     ANMODE=ANINFO,
     .                                     I1=ID,I2=EL2) 
            ENDIF
C
C--------   Initialisation of fram structure
C
            NFRAM = 1
            SLIPRING(I)%NFRAM = 1
            MY_ALLOCATE (SLIPRING(I)%FRAM,NFRAM)
C
            DO J=1,NFRAM
              SLIPRING(I)%FRAM(J)%UPDATE = 0
              SLIPRING(I)%FRAM(J)%ANCHOR_NODE = 0
              SLIPRING(I)%FRAM(J)%ORIENTATION_NODE = 0
              SLIPRING(I)%FRAM(J)%NODE = 0
              SLIPRING(I)%FRAM(J)%NODE_NEXT = 0
              SLIPRING(I)%FRAM(J)%NODE2_PREV = 0
              SLIPRING(I)%FRAM(J)%N_REMOTE_PROC = 0
              SLIPRING(I)%FRAM(J)%STRAND_DIRECTION = 1
              SLIPRING(I)%FRAM(J)%LOCKED = 0
              SLIPRING(I)%FRAM(J)%VECTOR = ZERO
              SLIPRING(I)%FRAM(J)%ORIENTATION_ANGLE = ZERO
              SLIPRING(I)%FRAM(J)%MATERIAL_FLOW = ZERO
              SLIPRING(I)%FRAM(J)%DFS = ZERO
              SLIPRING(I)%FRAM(J)%RESIDUAL_LENGTH = ZERO
              SLIPRING(I)%FRAM(J)%CURRENT_LENGTH = ZERO
              SLIPRING(I)%FRAM(J)%RINGSLIP = ZERO
              SLIPRING(I)%FRAM(J)%BETA = ZERO
              SLIPRING(I)%FRAM(J)%GAMMA = ZERO
              SLIPRING(I)%FRAM(J)%SLIP_FORCE = ZERO
              SLIPRING(I)%FRAM(J)%PREV_REF_LENGTH = ZERO
              SLIPRING(I)%FRAM(J)%INTVAR_STR1 = ZERO
              SLIPRING(I)%FRAM(J)%INTVAR_STR2 = ZERO
            ENDDO
C
C---------- Fill of fram structure
C
            NODE_ID = USR2SYS(NODE_ID,ITABM1,MESS,SLIPRING(I)%ID)
            IF (NODE_ID2 > 0) NODE_ID2 = USR2SYS(NODE_ID2,ITABM1,MESS,SLIPRING(I)%ID)                                                                                        
C
C---------
C
            SLIPRING(I)%FRAM(1)%ANCHOR_NODE = NODE_ID
            SLIPRING(I)%FRAM(1)%ORIENTATION_NODE = NODE_ID2
C          
            NODE1 = IXR(2,EL1_LOC)
            NODE2 = IXR(3,EL1_LOC)
            NODE3 = IXR(2,EL2_LOC)
            NODE4 = IXR(3,EL2_LOC)
C
            IF (NODE2 == NODE3) THEN
              SLIPRING(I)%FRAM(1)%NODE(1) = NODE1
              SLIPRING(I)%FRAM(1)%NODE(2) = NODE2
              SLIPRING(I)%FRAM(1)%NODE(3) = NODE4
            ELSEIF (NODE1 == NODE3) THEN
              SLIPRING(I)%FRAM(1)%NODE(1) = NODE2
              SLIPRING(I)%FRAM(1)%NODE(2) = NODE1
              SLIPRING(I)%FRAM(1)%NODE(3) = NODE4
            ELSEIF (NODE1 == NODE4) THEN
              SLIPRING(I)%FRAM(1)%NODE(1) = NODE2
              SLIPRING(I)%FRAM(1)%NODE(2) = NODE1
              SLIPRING(I)%FRAM(1)%NODE(3) = NODE3
            ELSEIF (NODE2 == NODE4) THEN
              SLIPRING(I)%FRAM(1)%NODE(1) = NODE1
              SLIPRING(I)%FRAM(1)%NODE(2) = NODE2
              SLIPRING(I)%FRAM(1)%NODE(3) = NODE3
            ENDIF
C
            IF(SLIPRING(I)%FRAM(1)%NODE(2) == 0) THEN
              IF ((EL1_LOC > 0).AND.(EL2_LOC > 0)) THEN
                CALL ANCMSG(MSGID=2003,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,I2=EL1,I3=EL2)
              ENDIF
            ELSEIF (SLIPRING(I)%FRAM(1)%NODE(2) == SLIPRING(I)%FRAM(1)%ANCHOR_NODE) THEN
              CALL ANCMSG(MSGID=2029,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,I2=ITAB(SLIPRING(I)%FRAM(1)%ANCHOR_NODE))
            ENDIF
C
            NODE1 = SLIPRING(I)%FRAM(1)%NODE(1)
            NODE2 = SLIPRING(I)%FRAM(1)%NODE(2)
            NODE3 = SLIPRING(I)%FRAM(1)%NODE(3)
            DIST1 = (X(1,NODE1)-X(1,NODE_ID))**2+(X(2,NODE1)-X(2,NODE_ID))**2+(X(3,NODE1)-X(3,NODE_ID))**2
            DIST2 = (X(1,NODE2)-X(1,NODE_ID))**2+(X(2,NODE2)-X(2,NODE_ID))**2+(X(3,NODE2)-X(3,NODE_ID))**2
            DIST3 = (X(1,NODE3)-X(1,NODE_ID))**2+(X(2,NODE3)-X(2,NODE_ID))**2+(X(3,NODE3)-X(3,NODE_ID))**2
C
C--         default tolerance
            TOLE_2 = EM10*(MAX(DIST1,DIST3))**2
C--         compatibility with random noise
            IF (NRAND > 0) THEN
              ALEA_MAX = ZERO
              DO J=1,NRAND
                ALEA_MAX = MAX(ALEA_MAX,ALEA(J))
              ENDDO 
              TOLE_2 = MAX(TOLE_2,TEN*ALEA_MAX*ALEA_MAX)
            ENDIF
C
C--         tolerance if node is very close to anchorage node
            IF (DIST2 <= TEN*TOLE_2) THEN
              X(1,NODE2) = X(1,NODE_ID)
              X(2,NODE2) = X(2,NODE_ID)
              X(3,NODE2) = X(3,NODE_ID)
              DIST2 = ZERO
            ENDIF
C
            IF ((EL1_LOC > 0).AND.(EL2_LOC > 0)) THEN
              IF(DIST2 > EM30) CALL ANCMSG(MSGID=2004,
     .                              MSGTYPE=MSGERROR,
     .                              ANMODE=ANINFO_BLIND_1,
     .                              I1=ID)
            ENDIF
C
            IF (NODE_ID2 > 0) THEN
C
              NN(1) = X(1,NODE_ID2) - X(1,NODE_ID)
              NN(2) = X(2,NODE_ID2) - X(2,NODE_ID)
              NN(3) = X(3,NODE_ID2) - X(3,NODE_ID)
              NORM = SQRT(MAX(EM30,NN(1)*NN(1)+NN(2)*NN(2)+NN(3)*NN(3)))
              NN(1) = NN(1) / NORM
              NN(2) = NN(2) / NORM
              NN(3) = NN(3) / NORM
C
              IF(NORM < EM20) CALL ANCMSG(MSGID=2018,
     .                            MSGTYPE=MSGERROR,
     .                            ANMODE=ANINFO_BLIND_1,
     .                            I1=ID)
C
              N1(1) = X(1,SLIPRING(I)%FRAM(1)%NODE(1)) - X(1,SLIPRING(I)%FRAM(1)%NODE(2))
              N1(2) = X(2,SLIPRING(I)%FRAM(1)%NODE(1)) - X(2,SLIPRING(I)%FRAM(1)%NODE(2))
              N1(3) = X(3,SLIPRING(I)%FRAM(1)%NODE(1)) - X(3,SLIPRING(I)%FRAM(1)%NODE(2))
              NORM = SQRT(MAX(EM30,N1(1)*N1(1)+N1(2)*N1(2)+N1(3)*N1(3)))
              N1(1) = N1(1) / NORM
              N1(2) = N1(2) / NORM
              N1(3) = N1(3) / NORM
C
              N2(1) = X(1,SLIPRING(I)%FRAM(1)%NODE(3)) - X(1,SLIPRING(I)%FRAM(1)%NODE(2))
              N2(2) = X(2,SLIPRING(I)%FRAM(1)%NODE(3)) - X(2,SLIPRING(I)%FRAM(1)%NODE(2))
              N2(3) = X(3,SLIPRING(I)%FRAM(1)%NODE(3)) - X(3,SLIPRING(I)%FRAM(1)%NODE(2))
              NORM = SQRT(MAX(EM30,N2(1)*N2(1)+N2(2)*N2(2)+N2(3)*N2(3)))
              N2(1) = N2(1) / NORM
              N2(2) = N2(2) / NORM
              N2(3) = N2(3) / NORM
C
              N3(1) = N1(2)*N2(3)-N1(3)*N2(2)
              N3(2) = N1(3)*N2(1)-N1(1)*N2(3)
              N3(3) = N1(1)*N2(2)-N1(2)*N2(1)
              NORM = SQRT(MAX(EM30,N3(1)*N3(1)+N3(2)*N3(2)+N3(3)*N3(3)))
              N3(1) = N3(1) / NORM
              N3(2) = N3(2) / NORM
              N3(3) = N3(3) / NORM
C
              SCAL = ABS(N3(1)*NN(1)+N3(2)*NN(2)+N3(3)*NN(3))
              SLIPRING(I)%FRAM(1)%ORIENTATION_ANGLE = ACOS(SCAL)
C
              WRITE(IOUT,1200) SLIPRING(I)%FRAM(1)%ORIENTATION_ANGLE   
C
            ENDIF
C
          ELSEIF (KEY(1:5)=='SHELL') THEN
C
C---------  SLIPRING/SHELL --------------------------------------------------------
C
            CALL HM_GET_INTV('EL_SET1', GR_SHEL1, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INTV('EL_SET2', GR_SHEL2, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INTV('Node_SET',GR_NOD, IS_AVAILABLE, LSUBMODEL)
C
            WRITE(IOUT,1300) ID,TRIM(TITR),GR_SHEL1,GR_SHEL2,GR_NOD,ISENS,FLOW_FLAG,A,ED_FACTOR,
     .                       IFUNC(1),IFUNC(2),FRICD,XSCALE1,YSCALE2,XSCALE2,
     .                       IFUNC(3),IFUNC(4),FRICS,XSCALE3,YSCALE4,XSCALE4
C
            INGR2USR => IGRNOD(1:NGRNOD)%ID
            GRN_LOC=NGR2USR(GR_NOD,INGR2USR,NGRNOD)
            NFRAM = IGRNOD(GRN_LOC)%NENTITY
C
            INGR2USR => IGRSH4N(1:NGRSHEL)%ID
            GRS1_LOC = NGR2USR(GR_SHEL1,INGR2USR,NGRSHEL)
            GRS2_LOC = NGR2USR(GR_SHEL2,INGR2USR,NGRSHEL)
C
C--------   Initialisation of fram structure
C
            SLIPRING(I)%NFRAM = NFRAM
            MY_ALLOCATE (SLIPRING(I)%FRAM,NFRAM)
            DO J=1,NFRAM
              SLIPRING(I)%FRAM(J)%UPDATE = 0
              SLIPRING(I)%FRAM(J)%ANCHOR_NODE = 0
              SLIPRING(I)%FRAM(J)%ORIENTATION_NODE = 0
              SLIPRING(I)%FRAM(J)%NODE = 0
              SLIPRING(I)%FRAM(J)%NODE_NEXT = 0
              SLIPRING(I)%FRAM(J)%NODE2_PREV = 0
              SLIPRING(I)%FRAM(J)%N_REMOTE_PROC = 0
              SLIPRING(I)%FRAM(J)%STRAND_DIRECTION = 1
              SLIPRING(I)%FRAM(J)%LOCKED = 0
              SLIPRING(I)%FRAM(J)%VECTOR = ZERO
              SLIPRING(I)%FRAM(J)%ORIENTATION_ANGLE = ZERO
              SLIPRING(I)%FRAM(J)%MATERIAL_FLOW = ZERO
              SLIPRING(I)%FRAM(J)%DFS = ZERO
              SLIPRING(I)%FRAM(J)%RESIDUAL_LENGTH = ZERO
              SLIPRING(I)%FRAM(J)%CURRENT_LENGTH = ZERO
              SLIPRING(I)%FRAM(J)%RINGSLIP = ZERO
              SLIPRING(I)%FRAM(J)%BETA = ZERO
              SLIPRING(I)%FRAM(J)%GAMMA = ZERO
              SLIPRING(I)%FRAM(J)%SLIP_FORCE = ZERO
              SLIPRING(I)%FRAM(J)%PREV_REF_LENGTH = ZERO
              SLIPRING(I)%FRAM(J)%INTVAR_STR1 = ZERO
              SLIPRING(I)%FRAM(J)%INTVAR_STR2 = ZERO
            ENDDO
C
C--------   Check of alignment of anchorage nodes
            MY_ALLOCATE(DIST,NFRAM)
            MY_ALLOCATE(JPERM,NFRAM)
            JPERM(1:NFRAM) = 0
            N_FIRST = IGRNOD(GRN_LOC)%ENTITY(1)
            N_LAST = IGRNOD(GRN_LOC)%ENTITY(IGRNOD(GRN_LOC)%NENTITY)
            DIST(1) = ZERO
            DIST(NFRAM) = (X(1,N_FIRST)-X(1,N_LAST))**2+(X(2,N_FIRST)-X(2,N_LAST))**2+(X(3,N_FIRST)-X(3,N_LAST))**2    
            NORM = SQRT(MAX(EM20,DIST(NFRAM)))
            VECT(1) = (X(1,N_FIRST)-X(1,N_LAST))/NORM
            VECT(2) = (X(2,N_FIRST)-X(2,N_LAST))/NORM
            VECT(3) = (X(3,N_FIRST)-X(3,N_LAST))/NORM
            DO J=2,NFRAM-1
              NJ = IGRNOD(GRN_LOC)%ENTITY(J)
              DIST(J) = (X(1,N_FIRST)-X(1,NJ))**2+(X(2,N_FIRST)-X(2,NJ))**2+(X(3,N_FIRST)-X(3,NJ))**2
              NORMJ = SQRT(MAX(EM20,DIST(J)))
              VECTJ(1) = (X(1,N_FIRST)-X(1,NJ))/NORMJ
              VECTJ(2) = (X(2,N_FIRST)-X(2,NJ))/NORMJ
              VECTJ(3) = (X(3,N_FIRST)-X(3,NJ))/NORMJ 
              SCAL =  ONE - ABS(VECT(1)*VECTJ(1)+VECT(2)*VECTJ(2)+VECT(3)*VECTJ(3))
              IF (ABS(SCAL) > EM08) THEN
                CALL ANCMSG(MSGID=2051,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=ID,I2=ITAB(NJ))
              ENDIF
            ENDDO

C--------   Check of distance between anchorage nodes (sorting by distance to first node)
            CALL MYQSORT(NFRAM,DIST,JPERM,IERROR)
            DO J=1,NFRAM-1
              IF (DIST(J)==DIST(J+1)) THEN
                NJ = IGRNOD(GRN_LOC)%ENTITY(JPERM(J))
                NJ_NEXT = IGRNOD(GRN_LOC)%ENTITY(JPERM(J+1))
                CALL ANCMSG(MSGID=2052,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=ID,I2=ITAB(NJ),I3=ITAB(NJ_NEXT))
              ENDIF
            ENDDO
            DEALLOCATE(DIST,JPERM)
C
C--------   Identification of node 2 for each frame
C
            MY_ALLOCATE(ELEM1_NOD,NFRAM)
            MY_ALLOCATE(ELEM2_NOD,NFRAM)
            MY_ALLOCATE(IPOS1_NOD,NFRAM)
            MY_ALLOCATE(IPOS2_NOD,NFRAM)
            MY_ALLOCATE(CORES1,NFRAM)
            MY_ALLOCATE(CORES2,NFRAM)
            MY_ALLOCATE(TAGNO,NUMNOD)
            CORES1(1:NFRAM) = 0
            CORES1(1:NFRAM) = 0
            IPOS1_NOD(1:NFRAM) = 0
            ELEM1_NOD(1:NFRAM) = 0
            IPOS2_NOD(1:NFRAM) = 0
            ELEM2_NOD(1:NFRAM) = 0
            TAGNO(1:NUMNOD) = 0
C
            DO J=1,NFRAM
              NJ = IGRNOD(GRN_LOC)%ENTITY(J)
C--           Search for closest node on element set 1 for each anchorage node 
              DIST_MIN = EP30
              DO K=1,IGRSH4N(GRS1_LOC)%NENTITY
                ISHELL = IGRSH4N(GRS1_LOC)%ENTITY(K)
                DO L = 1,4
                  NODE = IXC(1+L,ISHELL)
                  DISTN = (X(1,NODE)-X(1,NJ))**2+(X(2,NODE)-X(2,NJ))**2+(X(3,NODE)-X(3,NJ))**2
                  IF (DISTN < DIST_MIN) THEN
                    DIST_MIN = DISTN
                    CORES1(J) = NODE
                    ELEM1_NOD(J) = ISHELL
                    IPOS1_NOD(J) = L  
                  ENDIF
                ENDDO
                MID = IXC(1,ISHELL)
                IF (IPM(2,MID)/=119) THEN
                  CALL ANCMSG(MSGID=2074,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO_BLIND_1,
     .                        I1=IXC(NIXC,ISHELL),
     .                        PRMOD=MSG_CUMU)  
                ENDIF             
              ENDDO
              IF (CORES1(J) > 0) TAGNO(CORES1(J)) = 1   
C--           Search for closest node on element set 2 for each anchorage node 
              DIST_MIN = EP30
              DO K=1,IGRSH4N(GRS2_LOC)%NENTITY
                ISHELL = IGRSH4N(GRS2_LOC)%ENTITY(K)
                DO L = 1,4
                  NODE = IXC(1+L,ISHELL)
                  DISTN = (X(1,NODE)-X(1,NJ))**2+(X(2,NODE)-X(2,NJ))**2+(X(3,NODE)-X(3,NJ))**2
                  IF (DISTN < DIST_MIN) THEN
                    DIST_MIN = DISTN
                    CORES2(J) = NODE
                    ELEM2_NOD(J) = ISHELL
                    IPOS2_NOD(J) = L 
                  ENDIF
                ENDDO
                MID = IXC(1,ISHELL)
                IF (IPM(2,MID)/=119) THEN
                  CALL ANCMSG(MSGID=2074,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=IXC(NIXC,ISHELL),
     .                      PRMOD=MSG_CUMU)      
                ENDIF        
              ENDDO
              IF (CORES2(J) > 0) TAGNO(CORES2(J)) = 1  
C--
              IF (CORES1(J) /= CORES2(J)) THEN
                CALL ANCMSG(MSGID=2053,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=ID,I2=GR_SHEL1,I3=GR_SHEL2,I4=ITAB(NJ))
              ENDIF
C                    
            ENDDO
C        
            CALL ANCMSG(MSGID=2074,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,
     .                  PRMOD=MSG_PRINT)     
C
C---------- Fill of fram structure
C
            DO J=1,NFRAM
C
              NODE_ID = IGRNOD(GRN_LOC)%ENTITY(J)
              SLIPRING(I)%FRAM(J)%ANCHOR_NODE = NODE_ID
              SLIPRING(I)%FRAM(J)%ORIENTATION_NODE = 0
C
              NODE2 = CORES1(J)
              SLIPRING(I)%FRAM(J)%NODE(2) = NODE2
C
              ISHELL = ELEM1_NOD(J)
              IPOS = IPOS1_NOD(J)
              DO K=1,4
                IF ((K/=IPOS+2).AND.(K/=IPOS-2).AND.(TAGNO(IXC(K+1,ISHELL)) == 0)) NODE1 = IXC(K+1,ISHELL)
              ENDDO
              SLIPRING(I)%FRAM(J)%NODE(1) = NODE1
C
              ISHELL = ELEM2_NOD(J)
              IPOS = IPOS2_NOD(J)
              DO K=1,4
                IF ((K/=IPOS+2).AND.(K/=IPOS-2).AND.(TAGNO(IXC(K+1,ISHELL)) == 0)) NODE3 = IXC(K+1,ISHELL)
              ENDDO
              SLIPRING(I)%FRAM(J)%NODE(3) = NODE3            
C
              DIST1 = (X(1,NODE1)-X(1,NODE_ID))**2+(X(2,NODE1)-X(2,NODE_ID))**2+(X(3,NODE1)-X(3,NODE_ID))**2
              DIST2 = (X(1,NODE2)-X(1,NODE_ID))**2+(X(2,NODE2)-X(2,NODE_ID))**2+(X(3,NODE2)-X(3,NODE_ID))**2
              DIST3 = (X(1,NODE3)-X(1,NODE_ID))**2+(X(2,NODE3)-X(2,NODE_ID))**2+(X(3,NODE3)-X(3,NODE_ID))**2
C
C--           default tolerance
              TOLE_2 = EM10*(MAX(DIST1,DIST3))**2
C--           compatibility with random noise
              IF (NRAND > 0) THEN
                ALEA_MAX = ZERO
                DO K=1,NRAND
                  ALEA_MAX = MAX(ALEA_MAX,ALEA(K))
                ENDDO 
                TOLE_2 = MAX(TOLE_2,TEN*ALEA_MAX*ALEA_MAX)
              ENDIF
C
C--           tolerance if node is very close to anchorage node
              IF (DIST2 <= TEN*TOLE_2) THEN
                X(1,NODE2) = X(1,NODE_ID)
                X(2,NODE2) = X(2,NODE_ID)
                X(3,NODE2) = X(3,NODE_ID)
                DIST2 = ZERO
              ENDIF
C
              IF ((DIST2 > EM30).AND.(CORES1(J) == CORES2(J))) THEN
                CALL ANCMSG(MSGID=2054,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=ID,I2=ITAB(NODE_ID)) 
              ENDIF            
C
            ENDDO
C
            DEALLOCATE(ELEM1_NOD,IPOS1_NOD,ELEM2_NOD,IPOS2_NOD,CORES1,CORES2,TAGNO)   
C
          ENDIF
C
        ENDDO
C      
      ENDIF
C
      IF (IERR1 /= 0) THEN
         WRITE(IOUT,*)' ** ERROR IN MEMORY ALLOCATION'
         WRITE(ISTDO,*)' ** ERROR IN MEMORY ALLOCATION'
         CALL ARRET(2)
      ENDIF
C                         
C-------------------------------------
C     Recherche des ID doubles
C-------------------------------------
      CALL UDOUBLE(SLIP_ID,1,NSLIPRING,MESS,0,BID)
      RETURN
C
1000  FORMAT(/
     . '      SLIPRING DEFINITIONS '/
     . '      ---------------------- ')
1100  FORMAT(/5X,'SLIPRING SPRING ID ',I10,1X,A
     .       /5X,'FIRST SPRING ELEMENT . . . . . . . . . . .',I10
     .       /5X,'SECOND SPRING ELEMENT  . . . . . . . . . .',I10
     .       /5X,'ANCHORAGE NODE . . . . . . . . . . . . . .',I10
     .       /5X,'ORIENTATION NODE . . . . . . . . . . . . .',I10
     .       /5X,'SENSOR ID  . . . . . . . . . . . . . . . .',I10
     .       /5X,'FLOW FLAG  . . . . . . . . . . . . . . . .',I10
     .       /5X,'A. . . . . . . . . . . . . . . . . . . . .',1PG20.4
     .       /5X,'EXPONENTIAL DECAY FACTOR . . . . . . . . .',1PG20.4
     .       /5X,'FUNC1 - DYNAMIC FRIC FUNC VS TIME  . . . .',I10
     .       /5X,'FUNC2 - DYNAMIC FRIC FUNC VS NORMAL FORCE ',I10
     .       /5X,'DYNAMIC FRIC COEFFICIENT . . . . . . . . .',1PG20.4
     .       /5X,'FUNC1 ABCISSA SCALE FACTOR . . . . . . . .',1PG20.4
     .       /5X,'FUNC2 ORDINATE SCALE FACTOR  . . . . . . .',1PG20.4
     .       /5X,'FUNC2 ABCISSA SCALE FACTOR . . . . . . . .',1PG20.4
     .       /5X,'FUNC3 - STATIC FRIC FUNC VS TIME . . . . .',I10
     .       /5X,'FUNC4 - STATIC FRIC FUNC VS NORMAL FORCE .',I10
     .       /5X,'STATIC FRIC COEFFICIENT  . . . . . . . . .',1PG20.4
     .       /5X,'FUNC3 ABCISSA SCALE FACTOR . . . . . . . .',1PG20.4
     .       /5X,'FUNC4 ORDINATE SCALE FACTOR  . . . . . . .',1PG20.4
     .       /5X,'FUNC4 ABCISSA SCALE FACTOR . . . . . . . .',1PG20.4)
C
1200  FORMAT( 5X,'INITIAL ORIENTATION ANGLE (RAD)  . . . . .',1PG20.4)
C
1300  FORMAT(/5X,'SLIPRING SHELL ID ',I10,1X,A
     .       /5X,'FIRST ELEMENT GROUP  . . . . . . . . . . .',I10
     .       /5X,'SECOND ELEMENT GROUP . . . . . . . . . . .',I10
     .       /5X,'ANCHORAGE NODE GROUP . . . . . . . . . . .',I10
     .       /5X,'SENSOR ID  . . . . . . . . . . . . . . . .',I10
     .       /5X,'FLOW FLAG  . . . . . . . . . . . . . . . .',I10
     .       /5X,'A. . . . . . . . . . . . . . . . . . . . .',1PG20.4
     .       /5X,'EXPONENTIAL DECAY FACTOR . . . . . . . . .',1PG20.4
     .       /5X,'FUNC1 - DYNAMIC FRIC FUNC VS TIME  . . . .',I10
     .       /5X,'FUNC2 - DYNAMIC FRIC FUNC VS NORMAL FORCE ',I10
     .       /5X,'DYNAMIC FRIC COEFFICIENT . . . . . . . . .',1PG20.4
     .       /5X,'FUNC1 ABCISSA SCALE FACTOR . . . . . . . .',1PG20.4
     .       /5X,'FUNC2 ORDINATE SCALE FACTOR  . . . . . . .',1PG20.4
     .       /5X,'FUNC2 ABCISSA SCALE FACTOR . . . . . . . .',1PG20.4
     .       /5X,'FUNC3 - STATIC FRIC FUNC VS TIME . . . . .',I10
     .       /5X,'FUNC4 - STATIC FRIC FUNC VS NORMAL FORCE .',I10
     .       /5X,'STATIC FRIC COEFFICIENT  . . . . . . . . .',1PG20.4
     .       /5X,'FUNC3 ABCISSA SCALE FACTOR . . . . . . . .',1PG20.4
     .       /5X,'FUNC4 ORDINATE SCALE FACTOR  . . . . . . .',1PG20.4
     .       /5X,'FUNC4 ABCISSA SCALE FACTOR . . . . . . . .',1PG20.4)

      END SUBROUTINE HM_READ_SLIPRING
