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
Chd|  LECTRANSSUB                   source/model/submodel/lectranssub.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        EULER_MROT                    source/model/submodel/euler_mrot.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        POINTS_TO_FRAME               source/model/submodel/3points_to_frame.F
Chd|        RTRANSPOS                     source/model/submodel/rtranspos.F
Chd|        USRTOS                        source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE LECTRANSSUB(X      ,IGRNOD,ITAB,ITABM1,UNITAB,
     .                       RTRANS ,LSUBMODEL,IS_DYNA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr06_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "sphcom.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER ITAB(*),ITABM1(*)
      my_real
     .   X(3,*),RTRANS(NTRANSF,*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      INTEGER IS_DYNA
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,I0,I1,I2,I3,I4,I5,I6,
     .          N0,N1,N2,N3,N4,N5,N6,IERROR,
     .   J,IS,ID,UID,IGU,IGS,NN,NTRANS,STAT,ITRANSSUB,IBID,
     .   TAGNODSUB(NUMNOD),NTAG,CTAG,ISU,IDU,IDNOD,
     .   INUM,K,CPT,ID_TRANSSUB,NUMNUSR
      INTEGER :: WORK(70000)
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX,INDEX1,TAGNODSUB_TMP,IDNODSUB
!!      INTEGER, ALLOCATABLE, DIMENSION(:) :: ITG
      my_real
     .   LX,LY,LZ,TX,TY,TZ,R,S,RX,RY,RZ,SX,SY,SZ,ANGLE,AT,FAC_L
      my_real
     .   VR(3),X0(3),X1(3),X2(3),X3(3),X4(3),X5(3),X6(3),
     .   ROT(9),PP(3,3),QQ(3,3),P(3),NORM1, NORM2, NORM3, SCAL1, 
     .   SCAL2, SCAL3, EPS,XC(3)
      CHARACTER KEY*10,CART*ncharline,TITR*nchartitle,
     .   VERS_IN*ncharfield,STRING*ncharfield,SOLVERKEYWORD*ncharline
!
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NGR2USR,USRTOS
      EXTERNAL NGR2USR,USRTOS
C=======================================================================
      ISU = 0
      TAGNODSUB = 0
      NUMNUSR = 0

      IF(IS_DYNA /= 0)THEN
        CALL CPP_NODE_COUNT(NUMNUSR)
        ALLOCATE( INDEX(2*NUMNUSR) )
        DO I=1,2*NUMNUSR
         INDEX(I)=I
        ENDDO
        ALLOCATE( INDEX1(2*NUMNOD) )
        DO I=1,2*NUMNOD
         INDEX1(I)=I
        ENDDO
        ALLOCATE( TAGNODSUB_TMP(NUMNUSR) )
        DO I=1,NUMNUSR
         TAGNODSUB_TMP(I)=I
        ENDDO
        ALLOCATE( IDNODSUB(NUMNUSR) )
        DO I=1,NUMNUSR
         IDNODSUB(I)=I
        ENDDO 
      ENDIF

      FAC_L = ONE
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C      TAG SUBMODEL NODES DYNA
C-------------------------------------------------- 
      IF(IS_DYNA /= 0)THEN
        CALL CPP_NODE_SUB_TAG_DYNA(TAGNODSUB_TMP,IDNODSUB)

        CALL MY_ORDERS( 0, WORK, IDNODSUB, INDEX, NUMNUSR , 1)
        CALL MY_ORDERS( 0, WORK, ITAB, INDEX1, NUMNOD , 1)

        DO I=1,NUMNUSR
          TAGNODSUB(INDEX1(I)) = TAGNODSUB_TMP(INDEX(I))
        ENDDO
C--------------------------------------------------
C      TAG SUBMODEL NODES RADIOSS
C-------------------------------------------------- 
      ELSE
        CALL CPP_NODE_SUB_TAG(TAGNODSUB)
      ENDIF
C--------------------------------------------------
C      COUNT NUMBER TRANSFORM OPTIONS
C-------------------------------------------------- 
       CALL HM_OPTION_COUNT('TRANSFORM',NTRANS)
C-------------------------------------------------- 
      IF (NTRANS > 0) WRITE (IOUT,100)  
C--------------------------------------------------
C      START READING TRANSFORM OPTIONS
C-------------------------------------------------- 
      CALL HM_OPTION_START('TRANSFORM')  
C--------------------------------------------------
C BROWSING MODEL TRANSFORM 1->NTRANS
C--------------------------------------------------
      DO I=1,NTRANS
C--------------------------------------------------
C EXTRACT DATAS OF /TRANSFORM/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       UNIT_ID = UID,
     .                       OPTION_TITR = TITR,
     .                       KEYWORD2 = KEY)
        RTRANS(I,19) = ID
C--------------------------------------------------
        CALL HM_GET_INTV('GR_NODE',IGU,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('SUBMODEL',ID_TRANSSUB,IS_AVAILABLE,LSUBMODEL)
c---------------------
        ITRANSSUB = 0
        IF(ID_TRANSSUB/=0)THEN
          DO J=1,NSUBMOD
            IF (LSUBMODEL(J)%NOSUBMOD == ID_TRANSSUB) THEN
              ITRANSSUB = J  
              EXIT
            ENDIF
          ENDDO 
          IF(ITRANSSUB==0)THEN
          CALL ANCMSG(MSGID=1824,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1=TITR)
          END IF
        END IF
        RTRANS(I,1) = ITRANSSUB
c---------------------
        IF(IGU /= 0 .AND. ID_TRANSSUB /= 0) THEN
          CALL ANCMSG(MSGID=914,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1=TITR)
        ENDIF
c---------------------
        IF (ITRANSSUB == 0) CYCLE
c---------------------
        RTRANS(I,1) = ITRANSSUB
        IF (KEY(1:3) == 'TRA') THEN
C
          RTRANS(I,2) = 1
C
          CALL HM_GET_INTV('node1',N0,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('node2',N1,IS_AVAILABLE,LSUBMODEL)
C
          CALL HM_GET_FLOATV('translation_x',TX,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('translation_y',TY,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('translation_z',TZ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c
          IF (N0 > 0 .OR. N1 > 0) THEN
            I0 = USRTOS(N0,ITABM1)
            I1 = USRTOS(N1,ITABM1)
            IF (I0 == 0) THEN
              CALL ANCMSG(MSGID=694,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=N0)
            END IF
            IF (I1 == 0) THEN
              CALL ANCMSG(MSGID=694,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=N1)
            END IF
C
C TRANSFORMATION OF COORDS OF NODES N0 & N1
            IF(TAGNODSUB(I0) == ITRANSSUB) THEN
              DO J=1,I-1
                IF(RTRANS(J,1) == ITRANSSUB) CALL RTRANSPOS(X0,J,RTRANS)
              END DO
            END IF
            IF(TAGNODSUB(I1) == ITRANSSUB) THEN
              DO J=1,I-1
                IF(RTRANS(J,1) == ITRANSSUB) CALL RTRANSPOS(X1,J,RTRANS)
              END DO
            END IF
            TX = X1(1) - X0(1)
            TY = X1(2) - X0(2)
            TZ = X1(3) - X0(3)
          ELSE
            TX = TX * FAC_L
            TY = TY * FAC_L 
            TZ = TZ * FAC_L 
          ENDIF
          RTRANS(I,15) = TX
          RTRANS(I,16) = TY
          RTRANS(I,17) = TZ
          S = SQRT(TX*TX + TY*TY + TZ*TZ)
C
          ROT(1:9)=ZERO
          ROT(1) = ONE
          ROT(5) = ONE
          ROT(9) = ONE
C
          WRITE(IOUT,500) ID,ID_TRANSSUB   
          IF (N0 > 0 .AND. N1 > 0) WRITE(IOUT,200) N0,N1
          WRITE(IOUT,510) S,TX,TY,TZ
C----
        ELSEIF (KEY(1:3) == 'ROT') THEN
C
          RTRANS(I,2) = 2
C
          CALL HM_GET_INTV('node1',N0,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('node2',N1,IS_AVAILABLE,LSUBMODEL)
C
          CALL HM_GET_FLOATV('rotation_point1_x',X0(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('rotation_point1_y',X0(2),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('rotation_point1_z',X0(3),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('rotation_point2_x',X1(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('rotation_point2_y',X1(2),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('rotation_point2_z',X1(3),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('rotation_angle',ANGLE,IS_AVAILABLE,LSUBMODEL,UNITAB)
c---------------------
          IF (N0 > 0 .OR. N1 > 0) THEN
            I0 = USRTOS(N0,ITABM1)
            I1 = USRTOS(N1,ITABM1)
            IF (I0 == 0) THEN
              CALL ANCMSG(MSGID=694,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=N0)
            END IF
            IF (I1 == 0) THEN
              CALL ANCMSG(MSGID=694,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=N1)
            END IF
            X0(1) = X(1,I0)
            X0(2) = X(2,I0) 
            X0(3) = X(3,I0)
            X1(1) = X(1,I1)
            X1(2) = X(2,I1) 
            X1(3) = X(3,I1)
C
C TRANSFORMATION OF COORDS OF NODES N0 & N1
            IF(TAGNODSUB(I0) == ITRANSSUB) THEN
              DO J=1,I-1
                IF(RTRANS(J,1) == ITRANSSUB) CALL RTRANSPOS(X0,J,RTRANS)
              END DO
            END IF
            IF(TAGNODSUB(I1) == ITRANSSUB) THEN
              DO J=1,I-1
                IF(RTRANS(J,1) == ITRANSSUB) CALL RTRANSPOS(X1,J,RTRANS)
              END DO
            END IF
c
          ELSE
            X0(1) = X0(1) * FAC_L 
            X0(2) = X0(2) * FAC_L  
            X0(3) = X0(3) * FAC_L 
            X1(1) = X1(1) * FAC_L 
            X1(2) = X1(2) * FAC_L  
            X1(3) = X1(3) * FAC_L 
          ENDIF
          TX = X1(1) - X0(1)
          TY = X1(2) - X0(2)
          TZ = X1(3) - X0(3)
          S  = SQRT(TX*TX + TY*TY + TZ*TZ)
          AT = ANGLE * PI/HUNDRED80 /MAX(EM20,S)
          TX = TX * AT
          TY = TY * AT
          TZ = TZ * AT
          CALL EULER_MROT (TX,TY,TZ,ROT)
          DO J=1,9
            RTRANS(I,J+2) = ROT(J)
          ENDDO
          DO J=1,3
            RTRANS(I,J+11) = X0(J)
          ENDDO
          DO J=1,3
            RTRANS(I,J+14) = X0(J)
          ENDDO
C
          WRITE(IOUT,600) ID,ID_TRANSSUB
          IF (N0 > 0 .AND. N1 > 0) WRITE(IOUT,200) N0,N1
          WRITE(IOUT,610) X0(1),X0(2),X0(3),TX,TY,TZ,ANGLE
C----
        ELSEIF (KEY(1:6) == 'MATRIX') THEN
C
          RTRANS(I,2) = 3
C
          CALL HM_GET_FLOATV('vector_1_x',RTRANS(I,3),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('vector_1_y',RTRANS(I,6),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('vector_1_z',RTRANS(I,9),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('vector_2_x',RTRANS(I,4),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('vector_2_y',RTRANS(I,7),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('vector_2_z',RTRANS(I,10),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('vector_3_x',RTRANS(I,5),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('vector_3_y',RTRANS(I,8),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('vector_3_z',RTRANS(I,11),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('position_x',RTRANS(I,15),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('position_y',RTRANS(I,16),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('position_z',RTRANS(I,17),IS_AVAILABLE,LSUBMODEL,UNITAB)
C
          RTRANS(I,15) = RTRANS(I,15) * FAC_L
          RTRANS(I,16) = RTRANS(I,16) * FAC_L
          RTRANS(I,17) = RTRANS(I,17) * FAC_L
C
          EPS   = EM3
          NORM1 = SQRT(RTRANS(I,3)**2+RTRANS(I,6)**2+RTRANS(I,9)**2)
          NORM2 = SQRT(RTRANS(I,4)**2+RTRANS(I,7)**2+RTRANS(I,10)**2)
          NORM3 = SQRT(RTRANS(I,5)**2+RTRANS(I,8)**2+RTRANS(I,11)**2)
          SCAL1 = RTRANS(I,3)*RTRANS(I,4)+RTRANS(I,6)*RTRANS(I,7)+
     .            RTRANS(I,9)*RTRANS(I,10)
          SCAL2 = RTRANS(I,3)*RTRANS(I,5)+RTRANS(I,6)*RTRANS(I,8)+
     .            RTRANS(I,9)*RTRANS(I,11)
          SCAL3 = RTRANS(I,4)*RTRANS(I,5)+RTRANS(I,7)*RTRANS(I,8)+
     .            RTRANS(I,10)*RTRANS(I,11)
          IF(ABS(ONE-NORM1) > EPS .OR. ABS(ONE-NORM2) > EPS .OR.
     .       ABS(ONE-NORM3) > EPS .OR. 
     .      SCAL1 > (EPS * NORM1*NORM2) .OR. SCAL2 > (EPS * NORM1*NORM3)
     .      .OR. SCAL3 > (EPS * NORM2*NORM3))THEN
            CALL ANCMSG(MSGID=986,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO)
          ENDIF
c
          WRITE(IOUT,700) ID,ID_TRANSSUB
c
          WRITE(IOUT,710) 
     .         RTRANS(I,3),RTRANS(I,6),RTRANS(I,9),RTRANS(I,15),
     .         RTRANS(I,4),RTRANS(I,7),RTRANS(I,10),RTRANS(I,16),
     .         RTRANS(I,5),RTRANS(I,8),RTRANS(I,11),RTRANS(I,17)
C----
        ELSEIF (KEY(1:8) == 'POSITION') THEN
C
          RTRANS(I,2) = 4
C
          CALL HM_GET_INTV('node1',N1,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('node2',N2,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('node3',N3,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('node4',N4,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('node5',N5,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('node6',N6,IS_AVAILABLE,LSUBMODEL)
C
          CALL HM_GET_FLOATV('X_Point_1',X1(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Y_Point_1',X1(2),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Z_Point_1',X1(3),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('X_Point_2',X2(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Y_Point_2',X2(2),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Z_Point_2',X2(3),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('X_Point_3',X3(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Y_Point_3',X3(2),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Z_Point_3',X3(3),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('X_Point_4',X4(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Y_Point_4',X4(2),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Z_Point_4',X4(3),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('X_Point_5',X5(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Y_Point_5',X5(2),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Z_Point_5',X5(3),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('X_Point_6',X6(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Y_Point_6',X6(2),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Z_Point_6',X6(3),IS_AVAILABLE,LSUBMODEL,UNITAB)
C
          IF (N1 > 0 .OR. N2 > 0 .OR. N3 > 0 .OR. 
     .        N4 > 0 .OR. N5 > 0 .OR. N6 > 0) THEN
            I1 = USRTOS(N1,ITABM1)
            I2 = USRTOS(N2,ITABM1)
            I3 = USRTOS(N3,ITABM1)
            I4 = USRTOS(N4,ITABM1)
            I5 = USRTOS(N5,ITABM1)
            I6 = USRTOS(N6,ITABM1)
            IF (I1 == 0) THEN
              CALL ANCMSG(MSGID=694,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANSTOP,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=N1)
            END IF
            X1(1) = X(1,I1)
            X1(2) = X(2,I1) 
            X1(3) = X(3,I1)
            IF (I2 == 0) THEN
              CALL ANCMSG(MSGID=694,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANSTOP,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=N2)
            END IF
            X2(1) = X(1,I2)
            X2(2) = X(2,I2) 
            X2(3) = X(3,I2)
            IF (I3 == 0) THEN
              CALL ANCMSG(MSGID=694,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANSTOP,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=N3)
            END IF
            X3(1) = X(1,I3)
            X3(2) = X(2,I3) 
            X3(3) = X(3,I3)
            IF (I4 == 0) THEN
              CALL ANCMSG(MSGID=694,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANSTOP,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=N4)
            END IF
            X4(1) = X(1,I4)
            X4(2) = X(2,I4) 
            X4(3) = X(3,I4)
            IF (I5 == 0) THEN
              CALL ANCMSG(MSGID=694,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANSTOP,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=N5)
            END IF
            X5(1) = X(1,I5)
            X5(2) = X(2,I5) 
            X5(3) = X(3,I5)
            IF (I6 == 0) THEN
              CALL ANCMSG(MSGID=694,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANSTOP,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=N6)
            END IF
            X6(1) = X(1,I6)
            X6(2) = X(2,I6) 
            X6(3) = X(3,I6)
C
C TRANSFORMATION OF COORDS OF NODES N0 & N1
            IF(TAGNODSUB(I1) == ITRANSSUB) THEN
              DO J=1,I-1
                IF(RTRANS(J,1) == ITRANSSUB) CALL RTRANSPOS(X1,J,RTRANS)
              END DO
            END IF
            IF(TAGNODSUB(I2) == ITRANSSUB) THEN
              DO J=1,I-1
                IF(RTRANS(J,1) == ITRANSSUB) CALL RTRANSPOS(X2,J,RTRANS)
              END DO
            END IF
            IF(TAGNODSUB(I3) == ITRANSSUB) THEN
              DO J=1,I-1
                IF(RTRANS(J,1) == ITRANSSUB) CALL RTRANSPOS(X3,J,RTRANS)
              END DO
            END IF
            IF(TAGNODSUB(I4) == ITRANSSUB) THEN
              DO J=1,I-1
                IF(RTRANS(J,1) == ITRANSSUB) CALL RTRANSPOS(X4,J,RTRANS)
              END DO
            END IF
            IF(TAGNODSUB(I5) == ITRANSSUB) THEN
              DO J=1,I-1
                IF(RTRANS(J,1) == ITRANSSUB) CALL RTRANSPOS(X5,J,RTRANS)
              END DO
            END IF
            IF(TAGNODSUB(I6) == ITRANSSUB) THEN
              DO J=1,I-1
                IF(RTRANS(J,1) == ITRANSSUB) CALL RTRANSPOS(X6,J,RTRANS)
              END DO
            END IF
          ELSE
            X1(1) = X1(1) * FAC_L 
            X1(2) = X1(2) * FAC_L  
            X1(3) = X1(3) * FAC_L 
            X2(1) = X2(1) * FAC_L 
            X2(2) = X2(2) * FAC_L  
            X2(3) = X2(3) * FAC_L 
            X3(1) = X3(1) * FAC_L 
            X3(2) = X3(2) * FAC_L  
            X3(3) = X3(3) * FAC_L 
            X4(1) = X4(1) * FAC_L 
            X4(2) = X4(2) * FAC_L  
            X4(3) = X4(3) * FAC_L 
            X5(1) = X5(1) * FAC_L 
            X5(2) = X5(2) * FAC_L  
            X5(3) = X5(3) * FAC_L 
            X6(1) = X6(1) * FAC_L 
            X6(2) = X6(2) * FAC_L  
            X6(3) = X6(3) * FAC_L 
          ENDIF
C
          CALL POINTS_TO_FRAME(X1,X2,X3,PP,IERROR)
          IF(IERROR==1.OR.IERROR==3)THEN 
            CALL ANCMSG(MSGID=1866,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,C1=TITR)
          END IF
          IF(IERROR >= 2)THEN
            CALL ANCMSG(MSGID=1867,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,C1=TITR)
          END IF
          CALL POINTS_TO_FRAME(X4,X5,X6,QQ,IERROR)
          IF(IERROR == 1)THEN 
            CALL ANCMSG(MSGID=1868,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,C1=TITR)
C
            ROT(1:9)=ZERO
            ROT(1) = ONE
            ROT(5) = ONE
            ROT(9) = ONE
            DO J=1,9
              RTRANS(I,J+2)  = ROT(J)
            ENDDO
            RTRANS(I,12:14) = ZERO
            RTRANS(I,15:17) = ZERO
C
          ELSE
C
            IF(IERROR == 2)THEN
              CALL ANCMSG(MSGID=1869,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,C1=TITR)
            END IF
C
            ROT(1)=QQ(1,1)*PP(1,1)+QQ(1,2)*PP(1,2)+QQ(1,3)*PP(1,3)   ! QQ . Transpose(PP)
            ROT(4)=QQ(1,1)*PP(2,1)+QQ(1,2)*PP(2,2)+QQ(1,3)*PP(2,3)
            ROT(7)=QQ(1,1)*PP(3,1)+QQ(1,2)*PP(3,2)+QQ(1,3)*PP(3,3)
            ROT(2)=QQ(2,1)*PP(1,1)+QQ(2,2)*PP(1,2)+QQ(2,3)*PP(1,3)
            ROT(5)=QQ(2,1)*PP(2,1)+QQ(2,2)*PP(2,2)+QQ(2,3)*PP(2,3)
            ROT(8)=QQ(2,1)*PP(3,1)+QQ(2,2)*PP(3,2)+QQ(2,3)*PP(3,3)
            ROT(3)=QQ(3,1)*PP(1,1)+QQ(3,2)*PP(1,2)+QQ(3,3)*PP(1,3)
            ROT(6)=QQ(3,1)*PP(2,1)+QQ(3,2)*PP(2,2)+QQ(3,3)*PP(2,3)
            ROT(9)=QQ(3,1)*PP(3,1)+QQ(3,2)*PP(3,2)+QQ(3,3)*PP(3,3)
C
            DO J=1,9
              RTRANS(I,J+2)  = ROT(J)
            ENDDO
            DO J=1,3
              RTRANS(I,J+11) = X1(J)
            ENDDO
            DO J=1,3
              RTRANS(I,J+14) = X4(J)       ! Xnew = X4 + ROT(Xold-X1)
            ENDDO
C
          END IF
C
          WRITE(IOUT,800) ID,ID_TRANSSUB
c
          WRITE(IOUT,810)
     .         (RTRANS(I,K+11) , K=1,3),
     .         (RTRANS(I,K+14) , K=1,3),
     .         RTRANS(I,3),RTRANS(I,6), RTRANS(I,9),
     .         RTRANS(I,4),RTRANS(I,7),RTRANS(I,10),
     .         RTRANS(I,5),RTRANS(I,8),RTRANS(I,11)
        ENDIF
      ENDDO
C-------------------------
      IF(IS_DYNA /= 0)THEN
        IF (ALLOCATED(INDEX)) DEALLOCATE(INDEX)
        IF (ALLOCATED(INDEX1)) DEALLOCATE(INDEX1)
        IF (ALLOCATED(TAGNODSUB_TMP)) DEALLOCATE(TAGNODSUB_TMP)
        IF (ALLOCATED(IDNODSUB)) DEALLOCATE(IDNODSUB)
      ENDIF
C-----------------------
      RETURN
C-----------------------
 100  FORMAT(//
     .'  NODAL TRANSFORMATIONS       '/,
     .'  ---------------------- ')
 200  FORMAT(10X,'   NODES           N0   . . . . .= ',I10/,
     .       10X,'                   N1   . . . . .= ',I10)
 300  FORMAT(10X,'   CENTER NODE     N0   . . . . .= ',I10)
 500  FORMAT(/
     . '    SUBMODEL TRANSLATION, TRANSFORMATION ID   = ',I10/,                        
     . '       SUBMODEL ID. . . . . . . . . . . .= ',I10/,
     . '       TRANSLATION VECTOR :')
 510  FORMAT(10X,'   VALUE. . . . . . . . . . . . .= ',E20.13/,
     . '             COORDINATES     X. . . . . . .= ',E20.13/,
     . '                             Y. . . . . . .= ',E20.13/,
     . '                             Z. . . . . . .= ',E20.13)
 600  FORMAT(/
     . '    SUBMODEL ROTATION, TRANSFORMATION ID.     = ',I10/,                       
     . '       SUBMODEL ID. . . . . . . . . . . .= ',I10/,
     . '       ROTATION VECTOR: ')
 610  FORMAT(10X,'   CENTER          X. . . . . . .= ',E20.13/,
     . '                             Y. . . . . . .= ',E20.13/,
     . '                             Z. . . . . . .= ',E20.13/,
     . '             DIRECTION       X. . . . . . .= ',E20.13/,
     . '                             Y. . . . . . .= ',E20.13/,
     . '                             Z. . . . . . .= ',E20.13/,
     . '             ANGLE            . . . . . . .= ',E20.13)
 700  FORMAT(/
     . '   SUBMODEL MATRIX TRANSFORMATION, TRANSFORMATION ID.=   ',I10/,                       
     . '       SUBMODEL ID. . . . . . . . . . . .= ',I10/)
 710  FORMAT(4X,'MATRIX '/,
     .'  '/,
     . 17X,'M11',17X,'M12',17X,'M13',18X,'TX' /,
     .         4E20.13/,
     . 17X,'M21',17X,'M22',17X,'M23',18X,'TY' /,
     .         4E20.13/,
     . 17X,'M31',17X,'M32',17X,'M33',18X,'TZ' /,
     .         4E20.13/)
 800  FORMAT(/
     . '   SUBMODEL TRANSFORMATION WRT 6 POSITIONS',/, 
     . '       TRANSFORMATION ID. . . . . . . . . . . = ',I10/,                       
     . '       SUBMODEL ID. . . . . . . . . . . . . . = ',I10/)
 810  FORMAT(
     . '             CENTER N1       X1 . . . . . .= ',E20.13/,
     . '                             Y1 . . . . . .= ',E20.13/,
     . '                             Z1 . . . . . .= ',E20.13/,
     . '             CENTER N4       X4 . . . . . .= ',E20.13/,
     . '                             Y4 . . . . . .= ',E20.13/,
     . '                             Z4 . . . . . .= ',E20.13/,
     . '             ROTATION MATRIX . . . . . . . = ',/,
     . ' . . . . . . . . M11 . . . . . . . . M12 . . . . . . . . M13',/,
     .         3E20.13/,
     . ' . . . . . . . . M21 . . . . . . . . M22 . . . . . . . . M23',/,
     .         3E20.13/,
     . ' . . . . . . . . M31 . . . . . . . . M32 . . . . . . . . M33',/,
     .         3E20.13/)
 3000 FORMAT(/10X,'NEW NODE COORDINATES',14X,'X',24X,'Y',24X,'Z')
 3500 FORMAT( 17X,I10,3(5X,E20.13))
C-----------------------
      RETURN
      END SUBROUTINE LECTRANSSUB
