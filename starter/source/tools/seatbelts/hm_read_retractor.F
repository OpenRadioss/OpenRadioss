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
Chd|  HM_READ_RETRACTOR             source/tools/seatbelts/hm_read_retractor.F
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
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        GET_U_FUNC                    source/user_interface/uaccess.F
Chd|        NINTRI                        source/system/nintrr.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_RETRACTOR(LSUBMODEL,ITABM1,IXR,ITAB,UNITAB,
     .                             X,FUNC_ID,NOM_OPT,ALEA,IPM)
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
      INTEGER, INTENT(IN) ::  ITABM1(NUMNOD),IXR(NIXR,NUMELR),ITAB(NUMNOD),FUNC_ID(NFUNCT),IPM(NPROPMI,NUMMAT)
      INTEGER, INTENT(INOUT) :: NOM_OPT(LNOPT1,SNOM_OPT1)
      my_real, INTENT(IN) ::  ALEA(NRAND)
      my_real, INTENT(INOUT) ::  X(3,NUMNOD)
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ,DIMENSION(NRETRACTOR) :: RET_ID
      INTEGER               :: I,J,K,ID, UID, NODE_ID, EL_ID, IERR1
      INTEGER               :: NODE1,NODE2,EL_LOC,BID,ISENS(2),IFUNC(3),IFUNC_LOC(3),TENS_TYP,MID,MTYP
      my_real               :: FORCE,ELEM_SIZE,DIST1,DIST2,DIST3,PULL,YSCALE1,XSCALE1,XSCALE1_UNIT,YSCALE1_UNIT
      my_real               :: YSCALE2,XSCALE2,XSCALE2_UNIT,YSCALE2_UNIT,XX,DXDY,GET_U_FUNC,ALEA_MAX,TOLE_2
      CHARACTER             :: TITR*nchartitle, KEY2*ncharkey,MESS*40
      LOGICAL :: IS_AVAILABLE
      EXTERNAL GET_U_FUNC
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS,NINTRI
C=======================================================================
      DATA MESS/'RETRACTOR DEFINITION                     '/
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IERR1      = 0
C
      IF(NRETRACTOR > 0 ) THEN
C
        WRITE(IOUT,1000)
C
        MY_ALLOCATE (RETRACTOR,NRETRACTOR)
        DO I=1,NRETRACTOR
          RETRACTOR(I)%ID = 0
          RETRACTOR(I)%IDG = 0
          RETRACTOR(I)%UPDATE = 0
          RETRACTOR(I)%ANCHOR_NODE = 0
          RETRACTOR(I)%NODE = 0
          RETRACTOR(I)%NODE_NEXT = 0
          RETRACTOR(I)%STRAND_DIRECTION = 0
          RETRACTOR(I)%IFUNC = 0
          RETRACTOR(I)%ISENS = 0
          RETRACTOR(I)%TENS_TYP = 0
          RETRACTOR(I)%LOCKED = 0
          RETRACTOR(I)%PRETENS_ACTIV = 0
          RETRACTOR(I)%INACTI_NNOD = 0
          RETRACTOR(I)%INACTI_NNOD_MAX = 0
          RETRACTOR(I)%N_REMOTE_PROC=0
          RETRACTOR(I)%VECTOR = ZERO
          RETRACTOR(I)%ELEMENT_SIZE = ZERO
          RETRACTOR(I)%FORCE = ZERO
          RETRACTOR(I)%MATERIAL_FLOW = ZERO
          RETRACTOR(I)%RESIDUAL_LENGTH = ZERO
          RETRACTOR(I)%FAC = ZERO
          RETRACTOR(I)%PULLOUT = ZERO
          RETRACTOR(I)%UNLOCK_FORCE = ZERO
          RETRACTOR(I)%LOCK_PULL = ZERO
          RETRACTOR(I)%LOCK_OFFSET = ZERO
          RETRACTOR(I)%LOCK_YIELD_FORCE = ZERO
          RETRACTOR(I)%RINGSLIP = ZERO
          RETRACTOR(I)%PRETENS_TIME = ZERO
          RETRACTOR(I)%PRETENS_PULL = ZERO
          RETRACTOR(I)%PRETENS_PULLMAX = ZERO
          RETRACTOR(I)%RET_FORCE = ZERO
        ENDDO
C
        CALL HM_OPTION_START('/RETRACTOR')

        DO I = 1,NRETRACTOR 
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_TITR = TITR, OPTION_ID = ID, UNIT_ID = UID)
C
          NOM_OPT(1,I)=ID
          CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,I),LTITR)
C
          CALL HM_GET_INTV('EL_ID', EL_ID, IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_INTV('Node_ID', NODE_ID, IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_FLOATV('Elem_size', ELEM_SIZE, IS_AVAILABLE, LSUBMODEL,UNITAB)
C
          CALL HM_GET_INTV('Sens_ID1', ISENS(1), IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_FLOATV('Pullout', PULL, IS_AVAILABLE, LSUBMODEL,UNITAB)
          CALL HM_GET_INTV('Fct_ID1', IFUNC(1), IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_INTV('Fct_ID2', IFUNC(2), IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_FLOATV('Yscale1',YSCALE1,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Xscale1',XSCALE1,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
          CALL HM_GET_FLOATV_DIM('Yscale1',YSCALE1_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV_DIM('Xscale1',XSCALE1_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
          CALL HM_GET_INTV('Sens_ID2', ISENS(2), IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_INTV('Tens_typ', TENS_TYP, IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_FLOATV('Force', FORCE, IS_AVAILABLE, LSUBMODEL,UNITAB)
          CALL HM_GET_INTV('Fct_ID3', IFUNC(3), IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_FLOATV('Yscale2',YSCALE2,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Xscale2',XSCALE2,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
          CALL HM_GET_FLOATV_DIM('Yscale2',YSCALE2_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV_DIM('Xscale2',XSCALE2_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
          RET_ID(I) = ID
C
          IF (IFUNC(1) > 0) THEN
            IF (XSCALE1== ZERO) XSCALE1 = ONE*XSCALE1_UNIT
            IF (YSCALE1== ZERO) YSCALE1 = ONE*YSCALE1_UNIT    
          ENDIF
C
          IF (IFUNC(2) == 0) IFUNC(2) = IFUNC(1)
C
          IF (IFUNC(3) > 0) THEN
            IF (XSCALE2== ZERO) XSCALE2 = ONE*XSCALE2_UNIT
            IF (YSCALE2== ZERO) YSCALE2 = ONE*YSCALE2_UNIT    
          ENDIF
C
          WRITE(IOUT,1100) ID,TRIM(TITR),EL_ID,NODE_ID,ELEM_SIZE,ISENS(1),PULL,IFUNC(1),IFUNC(2),
     .                     XSCALE1,YSCALE1
C
          IF (ISENS(2) > 0) WRITE(IOUT,1200) ISENS(2),TENS_TYP,FORCE,IFUNC(3),XSCALE2,YSCALE2
C
          IF (FORCE == ZERO) FORCE = EP30
C
          NODE_ID = USR2SYS(NODE_ID,ITABM1,MESS,RETRACTOR(I)%ID)
          EL_LOC=NINTRI(EL_ID,IXR,NIXR,NUMELR,NIXR)
C
          IF(EL_LOC == 0) THEN
            CALL ANCMSG(MSGID=2008,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,I2=EL_ID)
          ELSE
            MTYP = 0
            MID = IXR(5,EL_LOC)
            IF (MID > 0) MTYP = IPM(2,MID)
            IF (MTYP /= 114) CALL ANCMSG(MSGID=2033,
     .                                   MSGTYPE=MSGERROR,
     .                                   ANMODE=ANINFO,
     .                                   I1=ID,I2=EL_ID) 
          ENDIF
C
C---------Check of sensors is done in creat_seatblet as sensors are not yet available
C
C---------Check of functions
C
          IFUNC_LOC(1:3) = 0
C              
          DO J=1,3       
            IF (IFUNC(J) > 0) THEN
              DO K=1,NFUNCT               
                IF (FUNC_ID(K) == IFUNC(J)) IFUNC_LOC(J) = K                                        
              ENDDO         
              IF(IFUNC_LOC(J) == 0) CALL ANCMSG(MSGID=2028,
     .                                   MSGTYPE=MSGERROR,
     .                                   ANMODE=ANINFO_BLIND_1,
     .                                   C1='FUNCTION',
     .                                   I1=ID,I2=IFUNC(J))
            ENDIF
          ENDDO
C
          IF ((ISENS(1) > 0).AND.(IFUNC(1)==0)) THEN
C--         function is mandatory for locking if sensor1 is input
              CALL ANCMSG(MSGID=2031,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID)
          ENDIF
C
          IF ((ISENS(2) > 0).AND.(IFUNC(3)==0)) THEN
C--         function is mandatory for pretensionin if sensor2 is input
              CALL ANCMSG(MSGID=2025,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,I1=ID)
          ENDIF
C
          RETRACTOR(I)%ID = ID
          RETRACTOR(I)%ANCHOR_NODE = NODE_ID
          RETRACTOR(I)%ELEMENT_SIZE = ELEM_SIZE
C
          RETRACTOR(I)%ISENS(1) = ISENS(1)
          RETRACTOR(I)%PULLOUT = PULL
          RETRACTOR(I)%IFUNC(1) = IFUNC_LOC(1)
          RETRACTOR(I)%IFUNC(2) = IFUNC_LOC(2)
          RETRACTOR(I)%FAC(1) = YSCALE1
          RETRACTOR(I)%FAC(2) = XSCALE1
C
          RETRACTOR(I)%ISENS(2) = ISENS(2)
          RETRACTOR(I)%TENS_TYP = TENS_TYP
          RETRACTOR(I)%FORCE = FORCE
          RETRACTOR(I)%IFUNC(3) = IFUNC_LOC(3)
          RETRACTOR(I)%FAC(3) = YSCALE2
          RETRACTOR(I)%FAC(4) = XSCALE2
C
          IF (RETRACTOR(I)%IFUNC(1)==0) THEN
            RETRACTOR(I)%UNLOCK_FORCE = RETRACTOR(I)%FAC(1)
          ELSE
C-          Force in unlock state is the first point of the curve
            XX = ZERO
            RETRACTOR(I)%UNLOCK_FORCE = RETRACTOR(I)%FAC(1)*GET_U_FUNC(RETRACTOR(I)%IFUNC(1),XX,DXDY)
          ENDIF
C          
          NODE1 = IXR(2,EL_LOC)
          NODE2 = IXR(3,EL_LOC)
C
          DIST1 = (X(1,NODE1)-X(1,NODE_ID))**2+(X(2,NODE1)-X(2,NODE_ID))**2+(X(3,NODE1)-X(3,NODE_ID))**2
          DIST2 = (X(1,NODE2)-X(1,NODE_ID))**2+(X(2,NODE2)-X(2,NODE_ID))**2+(X(3,NODE2)-X(3,NODE_ID))**2
C
C--       default tolerance
          TOLE_2 = EM10*RETRACTOR(I)%ELEMENT_SIZE*RETRACTOR(I)%ELEMENT_SIZE
C--       compatibility with random noise
          IF (NRAND > 0) THEN
            ALEA_MAX = ZERO
            DO J=1,NRAND
              ALEA_MAX = MAX(ALEA_MAX,ALEA(J))
            ENDDO 
            TOLE_2 = MAX(TOLE_2,TEN*ALEA_MAX*ALEA_MAX)
          ENDIF
C
C--       tolerance if node is very close to anchorage node
          IF ((DIST1 < DIST2).AND.(DIST1 <= TOLE_2)) THEN
            X(1,NODE1) = X(1,NODE_ID)
            X(2,NODE1) = X(2,NODE_ID)
            X(3,NODE1) = X(3,NODE_ID)
            DIST1 = ZERO
          ELSEIF (DIST2 <= TOLE_2) THEN
            X(1,NODE2) = X(1,NODE_ID)
            X(2,NODE2) = X(2,NODE_ID)
            X(3,NODE2) = X(3,NODE_ID)
            DIST2 = ZERO
          ENDIF
C
          DIST3 = (X(1,NODE2)-X(1,NODE1))**2+(X(2,NODE2)-X(2,NODE1))**2+(X(3,NODE2)-X(3,NODE1))**2
C
          IF (DIST1 < EM30) THEN
            RETRACTOR(I)%NODE(1) = NODE2 
            RETRACTOR(I)%NODE(2) = NODE1
            IF (RETRACTOR(I)%ELEMENT_SIZE == ZERO) RETRACTOR(I)%ELEMENT_SIZE = DIST2
            RETRACTOR(I)%VECTOR(1) = (X(1,NODE2)-X(1,NODE1))/SQRT(MAX(EM30,DIST3))
            RETRACTOR(I)%VECTOR(2) = (X(2,NODE2)-X(2,NODE1))/SQRT(MAX(EM30,DIST3))
            RETRACTOR(I)%VECTOR(3) = (X(3,NODE2)-X(3,NODE1))/SQRT(MAX(EM30,DIST3))
C--         retractor direction 2->1
            RETRACTOR(I)%STRAND_DIRECTION = -1          
          ELSEIF (DIST2 < EM30) THEN
            RETRACTOR(I)%NODE(1) = NODE1 
            RETRACTOR(I)%NODE(2) = NODE2
            IF (RETRACTOR(I)%ELEMENT_SIZE == ZERO) RETRACTOR(I)%ELEMENT_SIZE = DIST1
            RETRACTOR(I)%VECTOR(1) = (X(1,NODE1)-X(1,NODE2))/SQRT(MAX(EM30,DIST3))
            RETRACTOR(I)%VECTOR(2) = (X(2,NODE1)-X(2,NODE2))/SQRT(MAX(EM30,DIST3))
            RETRACTOR(I)%VECTOR(3) = (X(3,NODE1)-X(3,NODE2))/SQRT(MAX(EM30,DIST3))
C--         retractor direction 1->2
            RETRACTOR(I)%STRAND_DIRECTION = 1        
          ELSE
            CALL ANCMSG(MSGID=2009,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID)
          ENDIF
C
          IF (DIST3 < EM30) THEN
            CALL ANCMSG(MSGID=2022,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID)
          ENDIF
C
          IF (RETRACTOR(I)%NODE(2) == RETRACTOR(I)%ANCHOR_NODE) THEN
             CALL ANCMSG(MSGID=2030,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,I2=ITAB(RETRACTOR(I)%ANCHOR_NODE))
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
      CALL UDOUBLE(RET_ID,1,NRETRACTOR,MESS,0,BID)
      RETURN
C
1000  FORMAT(/
     . '      RETRACTOR/SPRING DEFINITIONS '/
     . '      ---------------------- ')
1100  FORMAT(/5X,'RETRACTOR ID ',I10,1X,A
     .       /5X,'CONNECTED SPRING ELEMENT . . . . . . . . .',I10
     .       /5X,'ANCHORAGE NODE . . . . . . . . . . . . . .',I10
     .       /5X,'ELEMENT SIZE . . . . . . . . . . . . . . .',1PG20.4
     .       /5X,'SENSOR ID1 . . . . . . . . . . . . . . . .',I10
     .       /5X,'PULLOUT BEFORE LOCKING . . . . . . . . . .',1PG20.4
     .       /5X,'FUNC1 - LOADING - FORCE VS PULLOUT . . . .',I10
     .       /5X,'FUNC2 - UNLOADING - FORCE VS PULLOUT . . .',I10
     .       /5X,'FUNC1/2 ABCISSA SCALE FACTOR . . . . . . .',1PG20.4
     .       /5X,'FUNC1/2 ORDINATE SCALE FACTOR. . . . . . .',1PG20.4)
1200  FORMAT( 5X,'PRETENSION :'
     .       /5X,'SENSOR ID2 . . . . . . . . . . . . . . . .',I10
     .       /5X,'PRETENSION TYPE. . . . . . . . . . . . . .',I10
     .       /5X,'MAXIMUM FORCE. . . . . . . . . . . . . . .',1PG20.4
     .       /5X,'FUNC3. . . . . . . . . . . . . . . . . . .',I10
     .       /5X,'FUNC3 ABCISSA SCALE FACTOR . . . . . . . .',1PG20.4
     .       /5X,'FUNC3 ORDINATE SCALE FACTOR  . . . . . . .',1PG20.4)
      END SUBROUTINE HM_READ_RETRACTOR
