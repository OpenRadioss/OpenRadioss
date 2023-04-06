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
Chd|  LECTRANS                      source/model/transformation/lectrans.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        EULER_MROT                    source/model/submodel/euler_mrot.F
Chd|        EULER_VROT                    source/model/submodel/euler_vrot.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        POINTS_TO_FRAME               source/model/submodel/3points_to_frame.F
Chd|        SUBROTPOINT                   source/model/submodel/subrot.F
Chd|        SUBROTVECT                    source/model/submodel/subrot.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        USRTOS                        source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE LECTRANS(X        ,IGRNOD ,ITAB   ,ITABM1,UNITAB,
     .                    LSUBMODEL,RTRANS )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER ITAB(*),ITABM1(*)
      my_real
     .   X(3,*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      my_real
     .   RTRANS(NTRANSF,*)
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,I0,I1,I2,I3,I4,I5,I6,
     .          N0,N1,N2,N3,N4,N5,N6,IERROR,
     .   J,IS,ID,UID,IGU,IGS,NN,NTRANS,STAT,
     .   IFLAGUNIT,ITRANSSUB,SUB_ID,K,
     .   IBID,CPT
      my_real
     .   LX,LY,LZ,TX,TY,TZ,R,S,RX,RY,RZ,SX,SY,SZ,ANGLE,AT,FAC_L,
     .   TRANS1,TRANS2,TRANS3,XP,YP,ZP
      my_real
     .   VR(3),X0(3),X1(3),X2(3),X3(3),X4(3),X5(3),X6(3),
     .   ROT(9),PP(3,3),QQ(3,3),P(3),NORM1, NORM2, NORM3, SCAL1, 
     .   SCAL2, SCAL3, EPS
      CHARACTER KEY*10
      CHARACTER MOT1*ncharfield,TITR*nchartitle,SOLVERKEYWORD*ncharline
!
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NGR2USR,USRTOS
      EXTERNAL NGR2USR,USRTOS
C--------------------------------------------------
C      COUNT NUMBER TRANSFORM OPTIONS
C-------------------------------------------------- 
      CALL HM_OPTION_COUNT('TRANSFORM',NTRANS)
      FAC_L = ONE
      IS_AVAILABLE = .FALSE.
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
     .                       SUBMODEL_ID = SUB_ID,
     .                       OPTION_TITR = TITR,
     .                       KEYWORD2 = KEY)
        RTRANS(I,19) = ID
C----
        IF (KEY(1:3) == 'TRA') THEN
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
          CALL HM_GET_INTV('GR_NODE',IGU,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('node1',N0,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('node2',N1,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('SUBMODEL',ITRANSSUB,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
          CALL HM_GET_FLOATV('translation_x',TX,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('translation_y',TY,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('translation_z',TZ,IS_AVAILABLE,LSUBMODEL,UNITAB)
C--------------------------------------------------
C APPLY //SUBMODEL TRANSFORMATIONs
C--------------------------------------------------
          IF(SUB_ID /= 0)
     .          CALL SUBROTVECT(TX,TY,TZ,RTRANS,SUB_ID,LSUBMODEL)
c---------------------
          IF (ITRANSSUB /= 0) CYCLE
c---------------------
          RTRANS(I,2) = 1
c---------------------
          INGR2USR => IGRNOD(1:NGRNOD)%ID
          IGS = NGR2USR(IGU,INGR2USR,NGRNOD)
          IF (IGS == 0) THEN           
            CALL ANCMSG(MSGID=1865,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1= ID,
     .                  C1= TITR,
     .                  I2= IGU)
          ENDIF
          RTRANS(I,18)=IGS
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
            TX = X(1,I1) - X(1,I0)
            TY = X(2,I1) - X(2,I0)
            TZ = X(3,I1) - X(3,I0)
          ELSE
            TX = TX * FAC_L
            TY = TY * FAC_L 
            TZ = TZ * FAC_L 
          ENDIF
          DO J=1,IGRNOD(IGS)%NENTITY
            IS=IGRNOD(IGS)%ENTITY(J)
            X(1,IS)=X(1,IS)+TX
            X(2,IS)=X(2,IS)+TY
            X(3,IS)=X(3,IS)+TZ
          ENDDO
          S = SQRT(TX*TX + TY*TY + TZ*TZ)
C
          WRITE(IOUT,500) ID,IGU   
          IF (N0 > 0 .AND. N1 > 0) WRITE(IOUT,200) N0,N1
          WRITE(IOUT,510) S,TX,TY,TZ
          IF (IPRI > 3) THEN                                          
            WRITE (IOUT,3000)                                            
            DO J=1,IGRNOD(IGS)%NENTITY
              IS=IGRNOD(IGS)%ENTITY(J)
              WRITE(IOUT,3500) ITAB(IS),X(1,IS),X(2,IS),X(3,IS)                                
            ENDDO
          ENDIF 
C----
        ELSEIF (KEY(1:3) == 'ROT') THEN
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
          CALL HM_GET_INTV('GR_NODE',IGU,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('node1',N0,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('node2',N1,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('SUBMODEL',ITRANSSUB,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
          CALL HM_GET_FLOATV('rotation_point1_x',X0(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('rotation_point1_y',X0(2),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('rotation_point1_z',X0(3),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('rotation_point2_x',X1(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('rotation_point2_y',X1(2),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('rotation_point2_z',X1(3),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('rotation_angle',ANGLE,IS_AVAILABLE,LSUBMODEL,UNITAB)
C--------------------------------------------------
C APPLY //SUBMODEL TRANSFORMATIONs
C--------------------------------------------------
          IF(SUB_ID /= 0)
     .    CALL SUBROTPOINT(X0(1),X0(2),X0(3),RTRANS,SUB_ID,LSUBMODEL)
c---------------------
          IF(SUB_ID /= 0)
     .    CALL SUBROTPOINT(X1(1),X1(2),X1(3),RTRANS,SUB_ID,LSUBMODEL)
c---------------------
          IF (ITRANSSUB /= 0) CYCLE
c---------------------
          RTRANS(I,2) = 2
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
          INGR2USR => IGRNOD(1:NGRNOD)%ID
          IGS = NGR2USR(IGU,INGR2USR,NGRNOD)
          IF (IGS == 0) THEN           
            CALL ANCMSG(MSGID=1865,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1= ID,
     .                  C1= TITR,
     .                  I2= IGU)
          ENDIF
          RTRANS(I,18)=IGS
          IF (ANGLE /= ZERO) THEN
            CALL EULER_MROT (TX,TY,TZ,ROT)
            DO J=1,IGRNOD(IGS)%NENTITY
              IS=IGRNOD(IGS)%ENTITY(J)
              CALL EULER_VROT (X0,X(1,IS),ROT)
            ENDDO
          ENDIF
C
          WRITE(IOUT,600) ID,IGU  
          IF (N0 > 0 .AND. N1 > 0) WRITE(IOUT,200) N0,N1
          WRITE(IOUT,610) X0(1),X0(2),X0(3),TX,TY,TZ,ANGLE
          IF (IPRI > 3) THEN                                           
            WRITE (IOUT,3000)                                         
            DO J=1,IGRNOD(IGS)%NENTITY
              IS=IGRNOD(IGS)%ENTITY(J)
              WRITE(IOUT,3500) ITAB(IS),X(1,IS),X(2,IS),X(3,IS)                                
            ENDDO                                              
          ENDIF
C----
        ELSEIF (KEY(1:3) == 'SYM') THEN
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
          CALL HM_GET_INTV('GR_NODE',IGU,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('node1',N0,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('node2',N1,IS_AVAILABLE,LSUBMODEL)
c          CALL HM_GET_INTV('SUBMODEL',ITRANSSUB,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
          CALL HM_GET_FLOATV('reflect_point1_x',X0(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('reflect_point1_y',X0(2),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('reflect_point1_z',X0(3),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('reflect_point2_x',X1(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('reflect_point2_y',X1(2),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('reflect_point2_z',X1(3),IS_AVAILABLE,LSUBMODEL,UNITAB)
C--------------------------------------------------
          IF(SUB_ID /= 0)
     .      CALL SUBROTPOINT(X0(1),X0(2),X0(3),RTRANS,SUB_ID,LSUBMODEL)
          IF(SUB_ID /= 0)
     .      CALL SUBROTPOINT(X1(1),X1(2),X1(3),RTRANS,SUB_ID,LSUBMODEL)
C--------------------------------------------------
          INGR2USR => IGRNOD(1:NGRNOD)%ID
          IGS = NGR2USR(IGU,INGR2USR,NGRNOD)
          IF (IGS == 0) THEN           
            CALL ANCMSG(MSGID=1865,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1= ID,
     .                  C1= TITR,
     .                  I2= IGU)
          ENDIF
          RTRANS(I,18)=IGS
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
          S = ONE/MAX(SQRT(TX*TX + TY*TY + TZ*TZ),EM20)
          TX = TX*S
          TY = TY*S
          TZ = TZ*S
          DO J=1,IGRNOD(IGS)%NENTITY
            IS=IGRNOD(IGS)%ENTITY(J)
            SX = X(1,IS) - X0(1)
            SY = X(2,IS) - X0(2)
            SZ = X(3,IS) - X0(3)
            S = SX*TX + SY*TY + SZ*TZ
            X(1,IS) = X(1,IS) - TWO*TX*S
            X(2,IS) = X(2,IS) - TWO*TY*S
            X(3,IS) = X(3,IS) - TWO*TZ*S
          ENDDO
C
          WRITE(IOUT,700) ID,IGU
          IF (N0 > 0 .AND. N1 > 0) WRITE(IOUT,200) N0,N1
          WRITE(IOUT,710) X0(1),X0(2),X0(3),TX,TY,TZ
          IF (IPRI > 3) THEN                                           
            WRITE (IOUT,3000)                                             
            DO J=1,IGRNOD(IGS)%NENTITY
              IS=IGRNOD(IGS)%ENTITY(J)
              WRITE(IOUT,3500) ITAB(IS),X(1,IS),X(2,IS),X(3,IS)                                
            ENDDO                                                
          ENDIF
C----
        ELSEIF (KEY(1:3) == 'SCA') THEN
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
          CALL HM_GET_INTV('GR_NODE',IGU,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('node1',N0,IS_AVAILABLE,LSUBMODEL)
c          CALL HM_GET_INTV('SUBMODEL',ITRANSSUB,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
          CALL HM_GET_FLOATV('scalefactor_x',TX,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('scalefactor_y',TY,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('scalefactor_z',TZ,IS_AVAILABLE,LSUBMODEL,UNITAB)
C--------------------------------------------------
          IF (TX == ZERO) TX = ONE
          IF (TY == ZERO) TY = ONE
          IF (TZ == ZERO) TZ = ONE
          IF(SUB_ID /= 0)
     .      CALL SUBROTVECT(TX,TY,TZ,RTRANS,SUB_ID,LSUBMODEL)
C--------------------------------------------------
          INGR2USR => IGRNOD(1:NGRNOD)%ID
          IGS = NGR2USR(IGU,INGR2USR,NGRNOD)
          IF (IGS == 0) THEN           
            CALL ANCMSG(MSGID=1865,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1= ID,
     .                  C1= TITR,
     .                  I2= IGU)
          ENDIF
          RTRANS(I,18)=IGS
          IF (N0 > 0) THEN
            I0 = USRTOS(N0,ITABM1)
            IF (I0 == 0) THEN
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
          ELSE
            X0(1) = ZERO
            X0(2) = ZERO 
            X0(3) = ZERO
          ENDIF
          DO J=1,IGRNOD(IGS)%NENTITY
            IS=IGRNOD(IGS)%ENTITY(J)
            X(1,IS) = X0(1) + (X(1,IS) - X0(1)) * TX
            X(2,IS) = X0(2) + (X(2,IS) - X0(2)) * TY
            X(3,IS) = X0(3) + (X(3,IS) - X0(3)) * TZ
          ENDDO
C
          WRITE(IOUT,800) ID,IGU
          IF (N0 > 0) WRITE(IOUT,300) N0
          WRITE(IOUT,810) TX,TY,TZ
          IF (IPRI > 3) THEN                                           
            WRITE (IOUT,3000)                                             
            DO J=1,IGRNOD(IGS)%NENTITY
              IS=IGRNOD(IGS)%ENTITY(J)
              WRITE(IOUT,3500) ITAB(IS),X(1,IS),X(2,IS),X(3,IS)                                
            ENDDO                                                
          ENDIF
C----
        ELSEIF (KEY(1:6) == 'MATRIX') THEN

          CALL HM_GET_INTV('GR_NODE',IGU,IS_AVAILABLE,LSUBMODEL)
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
          CALL HM_GET_INTV('SUBMODEL',ITRANSSUB,IS_AVAILABLE,LSUBMODEL)
c---------------------
          IF (ITRANSSUB /= 0) CYCLE
c---------------------
          RTRANS(I,2) = 3
c---------------------
          INGR2USR => IGRNOD(1:NGRNOD)%ID
          IGS = NGR2USR(IGU,INGR2USR,NGRNOD)
          IF (IGS == 0) THEN           
            CALL ANCMSG(MSGID=1865,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1= ID,
     .                  C1= TITR,
     .                  I2= IGU)
          ENDIF
          RTRANS(I,18)=IGS

          TRANS1 = TRANS1 * FAC_L
          TRANS2 = TRANS2 * FAC_L
          TRANS3 = TRANS3 * FAC_L
c
          EPS   = EM3
          NORM1 = SQRT(ROT(1)**2+ROT(4)**2+ROT(7)**2)
          NORM2 = SQRT(ROT(2)**2+ROT(5)**2+ROT(8)**2)
          NORM3 = SQRT(ROT(3)**2+ROT(6)**2+ROT(9)**2)
          SCAL1 = ROT(1)*ROT(2)+ROT(4)*ROT(5)+ROT(7)*ROT(8)
          SCAL2 = ROT(1)*ROT(3)+ROT(4)*ROT(6)+ROT(7)*ROT(9)
          SCAL3 = ROT(2)*ROT(3)+ROT(5)*ROT(6)+ROT(8)*ROT(9)
          IF(ABS(ONE-NORM1) > EPS .OR. ABS(ONE-NORM2) > EPS .OR.
     .       ABS(ONE-NORM3) > EPS .OR. 
     .      SCAL1 > (EPS * NORM1*NORM2) .OR. SCAL2 > (EPS * NORM1*NORM3)
     .      .OR. SCAL3 > (EPS * NORM2*NORM3))THEN
            CALL ANCMSG(MSGID=986,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO)
          ENDIF
c---------------------
          DO J=1,IGRNOD(IGS)%NENTITY
            IS=IGRNOD(IGS)%ENTITY(J)
            XP = ROT(1)*X(1,IS) + ROT(2)*X(2,IS) + ROT(3)*X(3,IS) 
     .           + TRANS1     
            YP = ROT(4)*X(1,IS) + ROT(5)*X(2,IS) + ROT(6)*X(3,IS) 
     .           + TRANS2   
            ZP = ROT(7)*X(1,IS) + ROT(8)*X(2,IS) + ROT(9)*X(3,IS) 
     .           + TRANS3                                  
            X(1,IS) = XP
            X(2,IS) = YP
            X(3,IS) = ZP
          ENDDO
c
          WRITE(IOUT,900) ID,IGU
c
          WRITE(IOUT,910) 
     .         ROT(1),ROT(2),ROT(3),TRANS1,
     .         ROT(4),ROT(5),ROT(6),TRANS2,
     .         ROT(7),ROT(8),ROT(9),TRANS3
C----
        ELSEIF (KEY(1:8) == 'POSITION') THEN
C--------------------------------------------------
          CALL HM_GET_INTV('GR_NODE',IGU,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('SUBMODEL',ITRANSSUB,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
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
c---------------------
          IF (ITRANSSUB /= 0) CYCLE
c---------------------
          RTRANS(I,2) = 4
C--------------------------------------------------
C APPLY //SUBMODEL TRANSFORMATIONs
C--------------------------------------------------
          IF(SUB_ID /= 0)THEN
            CALL SUBROTPOINT(X1(1),X1(2),X1(3),RTRANS,SUB_ID,LSUBMODEL)
            CALL SUBROTPOINT(X2(1),X2(2),X2(3),RTRANS,SUB_ID,LSUBMODEL)
            CALL SUBROTPOINT(X3(1),X3(2),X3(3),RTRANS,SUB_ID,LSUBMODEL)
            CALL SUBROTPOINT(X4(1),X4(2),X4(3),RTRANS,SUB_ID,LSUBMODEL)
            CALL SUBROTPOINT(X5(1),X5(2),X5(3),RTRANS,SUB_ID,LSUBMODEL)
            CALL SUBROTPOINT(X6(1),X6(2),X6(3),RTRANS,SUB_ID,LSUBMODEL)
          END IF
c---------------------
          INGR2USR => IGRNOD(1:NGRNOD)%ID
          IGS = NGR2USR(IGU,INGR2USR,NGRNOD)
          IF (IGS == 0) THEN           
            CALL ANCMSG(MSGID=1865,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1= ID,
     .                  C1= TITR,
     .                  I2= IGU)
          ENDIF
          RTRANS(I,18)=IGS
c---------------------
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
          END IF
C--------------------------------------------------
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
C--------------------------------------------------
          DO J=1,IGRNOD(IGS)%NENTITY
            K = IGRNOD(IGS)%ENTITY(J)
            XP = X(1,K) - X1(1)
            YP = X(2,K) - X1(2)
            ZP = X(3,K) - X1(3)
            X(1,K) = X4(1) + ROT(1)*XP + ROT(4)*YP + ROT(7)*ZP                        
            X(2,K) = X4(2) + ROT(2)*XP + ROT(5)*YP + ROT(8)*ZP                         
            X(3,K) = X4(3) + ROT(3)*XP + ROT(6)*YP + ROT(9)*ZP      
          END DO
C
          WRITE(IOUT,1000) ID,IGU
          WRITE(IOUT,1010)
     .         (RTRANS(I,K+11) , K=1,3),
     .         (RTRANS(I,K+14) , K=1,3),
     .         RTRANS(I,3),RTRANS(I,6), RTRANS(I,9),
     .         RTRANS(I,4),RTRANS(I,7),RTRANS(I,10),
     .         RTRANS(I,5),RTRANS(I,8),RTRANS(I,11)
C
          IF (IPRI > 3) THEN                                           
            WRITE (IOUT,3000)                                         
            DO J=1,IGRNOD(IGS)%NENTITY
              IS=IGRNOD(IGS)%ENTITY(J)
              WRITE(IOUT,3500) ITAB(IS),X(1,IS),X(2,IS),X(3,IS)                                
            ENDDO                                              
          ENDIF
        ENDIF
      ENDDO
C-----------------------
      RETURN
 999  CALL FREERR(3)
C-----------------------
 100  FORMAT(//
     .'  NODAL TRANSFORMATIONS       '/,
     .'  ---------------------- ')
 200  FORMAT(10X,'   NODES           N0   . . . . .= ',I10/,
     .       10X,'                   N1   . . . . .= ',I10)
 300  FORMAT(10X,'   CENTER NODE     N0   . . . . .= ',I10)
 500  FORMAT(/
     . '    NODAL TRANSLATION, TRANSFORMATION ID   = ',I10/,                        
     . '       NODE GROUP ID. . . . . . . . . . . .= ',I10/,
     . '       TRANSLATION VECTOR :')
 510  FORMAT(10X,'   VALUE. . . . . . . . . . . . .= ',E20.13/,
     . '             COORDINATES     X. . . . . . .= ',E20.13/,
     . '                             Y. . . . . . .= ',E20.13/,
     . '                             Z. . . . . . .= ',E20.13)
 600  FORMAT(/
     . '    NODAL ROTATION, TRANSFORMATION ID.     = ',I10/,                       
     . '       NODE GROUP ID. . . . . . . . . . . .= ',I10/,
     . '       ROTATION VECTOR: ')
 610  FORMAT(10X,'   CENTER          X. . . . . . .= ',E20.13/,
     . '                             Y. . . . . . .= ',E20.13/,
     . '                             Z. . . . . . .= ',E20.13/,
     . '             DIRECTION       X. . . . . . .= ',E20.13/,
     . '                             Y. . . . . . .= ',E20.13/,
     . '                             Z. . . . . . .= ',E20.13/,
     . '             ANGLE            . . . . . . .= ',E20.13)
 700  FORMAT(/
     . '    PLANE SYMMETRY, TRANSFORMATION ID = ',I10/,                        
     . '       NODE GROUP ID. . . . . . . . . . . .= ',I10/,
     . '       VECTOR ORTHOGONAL TO PLANE: ')
 710  FORMAT(10X,'   CENTER          X. . . . . . .= ',E20.13/,
     . '                             Y. . . . . . .= ',E20.13/,
     . '                             Z. . . . . . .= ',E20.13/,
     . '             DIRECTION       X. . . . . . .= ',E20.13/,
     . '                             Y. . . . . . .= ',E20.13/,
     . '                             Z. . . . . . .= ',E20.13)
 800  FORMAT(/
     . '    SCALING, TRANSFORMATION ID = ',I10/,
     . '       NODE GROUP ID. . . . . . . . . . . .= ',I10)
 810  FORMAT(10X,'   SCALE COEFF.    X. . . . . . .= ',E20.13/,
     . '                             Y. . . . . . .= ',E20.13/,
     . '                             Z. . . . . . .= ',E20.13)
 900  FORMAT(/
     . '    MATRIX TRANSFORMATION, TRANSFORMATION ID.=   ',I10/,                       
     . '       NODE GROUP ID. . . . . . . . . . . .= ',I10/)
 910  FORMAT(4X,'MATRIX '/,
     .'  '/,
     . 17X,'M11',17X,'M12',17X,'M13',18X,'TX' /,
     .         4E20.13/,
     . 17X,'M21',17X,'M22',17X,'M23',18X,'TY' /,
     .         4E20.13/,
     . 17X,'M31',17X,'M32',17X,'M33',18X,'TZ' /,
     .         4E20.13/)
 1000 FORMAT(/
     . '   SUBMODEL TRANSFORMATION WRT 6 POSITIONS',/, 
     . '       TRANSFORMATION ID. . . . . . . . . . . = ',I10/,                       
     . '       NODE GROUP ID. . . . . . . . . . . . . = ',I10)
 1010 FORMAT(
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
      END SUBROUTINE LECTRANS
