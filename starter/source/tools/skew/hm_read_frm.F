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
Chd|  HM_READ_FRM                   source/tools/skew/hm_read_frm.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ANODSET                       source/output/analyse/analyse_node.c
Chd|        FREERR                        source/starter/freform.F      
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|        SUBROTPOINT                   source/model/submodel/subrot.F
Chd|        SUBROTVECT                    source/model/submodel/subrot.F
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FRM(ISKN    ,X       ,ITAB     ,ITABM1  ,
     .                       XFRAME  ,LSUBMODEL,RTRANS ,NOM_OPT ,UNITAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "analyse_name.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com10_c.inc"
#include      "sphcom.inc"
#include      "units_c.inc"
#include      "titr_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER ISKN(LISKN,*), ITAB(*), ITABM1(*)
C     REAL ou REAL*8
      my_real
     .   X(3,*), XFRAME(NXFRAME,*),
     .   RTRANS(NTRANSF,*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IMOV, INOD, J, N1, N2, N3, K, NSK,
     .        IUN, JJ, SUB_ID,
     .        IDIR,IFLAGUNIT,ID,UID,CPT
C     REAL ou REAL*8
      my_real
     .   P(12), PNOR1, PNOR2, PNORM1, DET1, DET2, DET3, DET, PP,BID
      CHARACTER NOMFG*nchartitle
      CHARACTER TITR*nchartitle,MESS*40,MESSF*40,KEY*ncharkey
      CHARACTER DIR*ncharfield
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
      DATA IUN/1/
      DATA MESSF/'MOVING FRAME '/
      DATA NOMFG/'global reference frame                 '/
C=======================================================================
C
      DO I=1,9
        XFRAME(I,1) =ZERO
      ENDDO 
      XFRAME(1,1) =UN
      XFRAME(5,1) =UN
      XFRAME(9,1) =UN
      DO I=1,9
        XFRAME(18+I,1) =ZERO
      ENDDO
      XFRAME(18+1,1) =UN
      XFRAME(18+5,1) =UN
      XFRAME(18+9,1) =UN
C
      JJ=(NUMSKW+1)+MIN(IUN,NSPCOND)*NUMSPH+1+NSUBMOD
      ISKN(1,JJ)=0
      ISKN(2,JJ)=0
      ISKN(3,JJ)=0
      ISKN(5,JJ)=0
C     Global Frame ID is set to -1
      ISKN(4,JJ)=-1
      NOM_OPT(1,NUMSKW+2)=-1
      CALL FRETITL(NOMFG,NOM_OPT(LNOPT1-LTITR+1,
     .                           NUMSKW+2),LTITR)
C
      IF(NUMFRAM.EQ.0)GOTO 900
C--------------------------------------------------
C START BROWSING MODEL PROPERTIES
C--------------------------------------------------
      CALL HM_OPTION_START('/FRAME')
      I = 0
C--------------------------------------------------
C BROWSING MODEL PROPERTIES 1->HM_NUMGEO
C--------------------------------------------------
      DO CPT=1,NUMFRAM
        I = I + 1
        JJ=(NUMSKW+1)+MIN(IUN,NSPCOND)*NUMSPH+I+1+NSUBMOD
C--------------------------------------------------
C EXTRACT DATAS OF /FRAME/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       UNIT_ID = UID,
     .                       SUBMODEL_ID = SUB_ID,
     .                       OPTION_TITR = TITR,
     .                       KEYWORD2 = KEY)
C
        NOM_OPT(1,NUMSKW+2+I)=ID
        CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,
     .                            NUMSKW+2+I),LTITR)
C
        IFLAGUNIT = 0
        DO J=1,NUNITS
          IF (UNITAB%UNIT_ID(J) == UID) THEN                 
            IFLAGUNIT = 1
            EXIT
          ENDIF
        ENDDO
        IF (UID.NE.0.AND.IFLAGUNIT.EQ.0) THEN
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I2=UID,I1=ID,C1='REFERENCE FRAME',
     .                C2='REFERENCE FRAME',
     .                C3=TITR) 
        ENDIF
C
        INOD  =0
        IMOV  =0
C
        IF (KEY(1:3).EQ.'FIX') THEN
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
          CALL HM_GET_FLOATV('globaloriginx',P(10),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('globaloriginy',P(11),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('globaloriginz',P(12),IS_AVAILABLE,LSUBMODEL,UNITAB)

          CALL HM_GET_FLOATV('globalyaxisx',P(4),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('globalyaxisy',P(5),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('globalyaxisz',P(6),IS_AVAILABLE,LSUBMODEL,UNITAB)

          CALL HM_GET_FLOATV('globalzaxisx',P(7),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('globalzaxisy',P(8),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('globalzaxisz',P(9),IS_AVAILABLE,LSUBMODEL,UNITAB)
C
        ELSEIF (KEY(1:4).EQ.'MOV2') THEN
          IMOV=2
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
          CALL HM_GET_INTV('originnodeid',N1,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('axisnodeid',N2,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('planenodeid',N3,IS_AVAILABLE,LSUBMODEL)
C
        ELSEIF (KEY(1:3).EQ.'MOV') THEN
          IMOV=1
          IDIR = 1
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
          CALL HM_GET_INTV('originnodeid',N1,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('axisnodeid',N2,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('planenodeid',N3,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (STRING)
C--------------------------------------------------
          CALL HM_GET_STRING('DIR',DIR,ncharfield,IS_AVAILABLE)
          DO K = 1,LFIELD
            IF(DIR(K:K) == 'X'.OR.DIR(K:K) == 'x')IDIR = 1
            IF(DIR(K:K) == 'Y'.OR.DIR(K:K) == 'Y')IDIR = 2
            IF(DIR(K:K) == 'Z'.OR.DIR(K:K) == 'Z')IDIR = 3
          ENDDO
          ISKN(6,JJ)=IDIR
C
        ELSEIF (KEY(1:3).EQ.'NOD') THEN
C       Node defined moving frame
          INOD=1
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
          CALL HM_GET_INTV('originnodeid',N1,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('axisnodeid',N2,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('planenodeid',N3,IS_AVAILABLE,LSUBMODEL)
          IF (N2.EQ.0 .OR. N3.EQ.0) THEN
            INOD=2
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
            CALL HM_GET_FLOATV('globalyaxisx',P(4),IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_FLOATV('globalyaxisy',P(5),IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_FLOATV('globalyaxisz',P(6),IS_AVAILABLE,LSUBMODEL,UNITAB)
C
            CALL HM_GET_FLOATV('globalzaxisx',P(7),IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_FLOATV('globalzaxisy',P(8),IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_FLOATV('globalzaxisz',P(9),IS_AVAILABLE,LSUBMODEL,UNITAB)
C
            IF(SUB_ID .NE. 0)
     .          CALL SUBROTVECT(P(4),P(5),P(6),RTRANS,SUB_ID,LSUBMODEL)
            IF(SUB_ID .NE. 0)
     .          CALL SUBROTVECT(P(7),P(8),P(9),RTRANS,SUB_ID,LSUBMODEL)
          ENDIF
        ENDIF
        ISKN(4,JJ)=ID
C----------------
C     FRAME MOBILE (CALCUL DE LA POSITION INITIALE)
C----------------
        IF(IMOV.EQ.1)THEN
          N1=USR2SYS(N1,ITABM1,MESSF,ID)
          N2=USR2SYS(N2,ITABM1,MESSF,ID)
          CALL ANODSET(N1, CHECK_USED)
          CALL ANODSET(N2, CHECK_USED)
          CALL IFRONTPLUS(N1,1)
          CALL IFRONTPLUS(N2,1)       
          ISKN(1,JJ)=N1
          ISKN(2,JJ)=N2
          ISKN(5,JJ)=IMOV
C-----------------
C     CALCUL DE X' et Y0'
C-----------------
          IF(N2D.EQ.0)THEN
c
            IF (IDIR == 1) THEN
              P(1)=X(1,N2)-X(1,N1)
              P(2)=X(2,N2)-X(2,N1)
              P(3)=X(3,N2)-X(3,N1)
            ELSEIF(IDIR == 2) THEN
              P(4)=X(1,N2)-X(1,N1)
              P(5)=X(2,N2)-X(2,N1)
              P(6)=X(3,N2)-X(3,N1)
            ELSEIF(IDIR == 3) THEN
              P(7)=X(1,N2)-X(1,N1)
              P(8)=X(2,N2)-X(2,N1)
              P(9)=X(3,N2)-X(3,N1)
            ENDIF
c
            N3=USR2SYS(N3,ITABM1,MESSF,ID)
            CALL ANODSET(N3, CHECK_USED)
            CALL IFRONTPLUS(N3,1)
            ISKN(3,JJ)=N3
C
            IF (IDIR == 1) THEN
              P(4)=X(1,N3)-X(1,N1)
              P(5)=X(2,N3)-X(2,N1)
              P(6)=X(3,N3)-X(3,N1)
            ELSEIF (IDIR == 2) THEN
              P(7)=X(1,N3)-X(1,N1)
              P(8)=X(2,N3)-X(2,N1)
              P(9)=X(3,N3)-X(3,N1)
            ELSEIF (IDIR == 3) THEN
              P(1)=X(1,N3)-X(1,N1)
              P(2)=X(2,N3)-X(2,N1)
              P(3)=X(3,N3)-X(3,N1)
            ENDIF
C
            P(10)=X(1,N1)
            P(11)=X(2,N1)
            P(12)=X(3,N1)
          ELSE
            P(1)=UN
            P(2)=ZERO
            P(3)=ZERO
C
            P(4)=X(1,N2)-X(1,N1)
            P(5)=X(2,N2)-X(2,N1)
            P(6)=X(3,N2)-X(3,N1)
C
            P(10)=X(1,N1)
            P(11)=X(2,N1)
             P(12)=X(3,N1)
          ENDIF
C----------------
C     TESTS DE CONSISTANCE
C----------------
          IF (IDIR == 1) PNOR1=SQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
          IF (IDIR == 2) PNOR1=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
          IF (IDIR == 3) PNOR1=SQRT(P(7)*P(7)+P(8)*P(8)+P(9)*P(9))
          IF(PNOR1.LT.1.E-20) THEN
            CALL ANCMSG(MSGID=162,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I2=ITAB(N1),
     .                  I1=ID,C1=TITR,
     .                  I3=ITAB(N2))
            RETURN
          ENDIF
C     CALCUL DE COLINEARITE DES VECTEURS N1N2 ET N1N3
          IF (IDIR == 1) THEN
            PNOR2=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
            IF(PNOR2.GT.EM20) THEN
              PNORM1=UN/(PNOR1*PNOR2)
              DET1=ABS((P(1)*P(5)-P(2)*P(4))*PNORM1)
              DET2=ABS((P(1)*P(6)-P(3)*P(4))*PNORM1)
              DET3=ABS((P(2)*P(6)-P(3)*P(5))*PNORM1)
              DET= MAX(DET1,DET2,DET3)
            ELSE
              DET=ZERO
            ENDIF
            IF(DET.LT.EM5) THEN
              CALL ANCMSG(MSGID=163,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,C1=TITR)
              IF(ABS(P(2)).GT.EM5) THEN
                P(4)=ABS(P(1))+DIX
              ELSE
                P(5)=DIX
              ENDIF
            ENDIF
          ELSEIF (IDIR == 2) THEN
            PNOR2=SQRT(P(7)*P(7)+P(8)*P(8)+P(9)*P(9))
            IF(PNOR2.GT.EM20) THEN
              PNORM1=UN/(PNOR1*PNOR2)
              DET1=ABS((P(4)*P(8)-P(5)*P(7))*PNORM1)
              DET2=ABS((P(4)*P(9)-P(6)*P(7))*PNORM1)
              DET3=ABS((P(5)*P(9)-P(6)*P(8))*PNORM1)
              DET= MAX(DET1,DET2,DET3)
            ELSE
              DET=ZERO
            ENDIF
            IF(DET.LT.EM5) THEN
              CALL ANCMSG(MSGID=163,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,C1=TITR)
              IF(ABS(P(5)).GT.EM5) THEN
                P(7)=ABS(P(4))+DIX
              ELSE
                P(8)=DIX
              ENDIF
            ENDIF
          ELSEIF (IDIR == 3) THEN
            PNOR2=SQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
            IF(PNOR2.GT.EM20) THEN
              PNORM1=UN/(PNOR1*PNOR2)
              DET1=ABS((P(7)*P(2)-P(8)*P(1))*PNORM1)
              DET2=ABS((P(7)*P(3)-P(9)*P(1))*PNORM1)
              DET3=ABS((P(8)*P(3)-P(9)*P(2))*PNORM1)
              DET= MAX(DET1,DET2,DET3)
            ELSE
              DET=ZERO
            ENDIF
            IF(DET.LT.EM5) THEN
              CALL ANCMSG(MSGID=163,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,C1=TITR)
              IF(ABS(P(5)).GT.EM5) THEN
                P(1)=ABS(P(7))+DIX
              ELSE
                P(2)=DIX
              ENDIF
            ENDIF
          ENDIF
C-----------------
C     CALCUL DE Z'
C-----------------
          IF (IDIR == 1) THEN
            P(7)=P(2)*P(6)-P(3)*P(5)
            P(8)=P(3)*P(4)-P(1)*P(6)
            P(9)=P(1)*P(5)-P(2)*P(4)
          ELSEIF (IDIR == 2) THEN
            P(1)=P(5)*P(9)-P(6)*P(8)
            P(2)=P(6)*P(7)-P(4)*P(9)
            P(3)=P(4)*P(8)-P(5)*P(7)
          ELSEIF (IDIR == 3) THEN
            P(4)=P(8)*P(3)-P(9)*P(2)
            P(5)=P(9)*P(1)-P(7)*P(3)
            P(6)=P(7)*P(2)-P(8)*P(1)
          ENDIF
C-----------------
C     CALCUL DE Y'
C-----------------
          IF (IDIR == 1) THEN
            P(4)=P(8)*P(3)-P(9)*P(2)
            P(5)=P(9)*P(1)-P(7)*P(3)
            P(6)=P(7)*P(2)-P(8)*P(1)
          ELSEIF (IDIR == 2) THEN
            P(7)=P(2)*P(6)-P(3)*P(5)
            P(8)=P(3)*P(4)-P(1)*P(6)
            P(9)=P(1)*P(5)-P(2)*P(4)
          ELSEIF (IDIR == 3) THEN
            P(1)=P(5)*P(9)-P(6)*P(8)
            P(2)=P(6)*P(7)-P(4)*P(9)
            P(3)=P(4)*P(8)-P(5)*P(7)
          ENDIF
C----------------
C     FRAME MOV2
C----------------
        ELSEIF (IMOV == 2) THEN
          N1=USR2SYS(N1,ITABM1,MESS,ID)
          N2=USR2SYS(N2,ITABM1,MESS,ID)
          N3=USR2SYS(N3,ITABM1,MESS,ID)
          CALL ANODSET(N1, CHECK_USED)
          CALL ANODSET(N2, CHECK_USED)
          CALL ANODSET(N3, CHECK_USED)
          CALL IFRONTPLUS(N1,1)
          CALL IFRONTPLUS(N2,1)
          CALL IFRONTPLUS(N3,1)
          ISKN(1,JJ)=N1
          ISKN(2,JJ)=N2
          ISKN(3,JJ)=N3
          ISKN(5,JJ)=IMOV
          P(7)=X(1,N2)-X(1,N1)
          P(8)=X(2,N2)-X(2,N1)
          P(9)=X(3,N2)-X(3,N1)
          P(1)=X(1,N3)-X(1,N1)
          P(2)=X(2,N3)-X(2,N1)
          P(3)=X(3,N3)-X(3,N1)
C-----------------
C       CALCUL DE Y = Z x X'
C-----------------
          P(4)=P(8)*P(3)-P(9)*P(2)
          P(5)=P(9)*P(1)-P(7)*P(3)
          P(6)=P(7)*P(2)-P(8)*P(1)
C-----------------
C       CALCUL DE X = Y x Z
C-----------------
          P(1)=P(5)*P(9)-P(6)*P(8)
          P(2)=P(6)*P(7)-P(4)*P(9)
          P(3)=P(4)*P(8)-P(5)*P(7)
C-----------------
C       ORIGINE
C-----------------
          P(10)=X(1,N1)
          P(11)=X(2,N1)
          P(12)=X(3,N1)
C----------------
C       TESTS DE CONSISTANCE
C----------------
          PNOR1=SQRT(P(7)*P(7)+P(8)*P(8)+P(9)*P(9))
          IF (PNOR1 < EM20) THEN
            CALL ANCMSG(MSGID=162,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I2=ITAB(N1),
     .                  I1=ID,C1=TITR,
     .                  I3=ITAB(N2))
          ENDIF
C       CALCUL DE COLINEARITE DES VECTEURS N1N2 ET N1N3
          PNOR2=SQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
          IF (PNOR2 > EM20) THEN
            PNORM1=UN/(PNOR1*PNOR2)
            DET1=ABS((P(8)*P(3)-P(9)*P(2))*PNORM1)
            DET2=ABS((P(9)*P(1)-P(7)*P(3))*PNORM1)
            DET3=ABS((P(7)*P(2)-P(8)*P(1))*PNORM1)
            DET= MAX(DET1,DET2,DET3)
          ELSE
            DET=ZERO
          ENDIF
          IF (DET < EM5) THEN
            CALL ANCMSG(MSGID=163,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,C1=TITR)
            IF(ABS(P(2)) < EM5) THEN
              P(4)=ABS(P(1))+DIX
            ELSE
              P(5)=DIX
            ENDIF
          ENDIF
C----------------
C     MOVING FRAME ATTACHED TO A NODE
C----------------
        ELSEIF (INOD.GE.1) THEN
          IF (N1.LE.0) THEN
            CALL ANCMSG(MSGID=900,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=SUB_ID,
     .                  C1=TITR,
     .                  I2=N1)
          ENDIF
          IF (N1.NE.0) THEN
            N1=USR2SYS(N1,ITABM1,MESSF,ID)
            CALL ANODSET(N1, CHECK_USED)
	    CALL IFRONTPLUS(N1,1)
          ENDIF
          IF (N2.NE.0) THEN
            N2=USR2SYS(N2,ITABM1,MESSF,ID)
            CALL ANODSET(N2, CHECK_USED)
	    CALL IFRONTPLUS(N2,1)
          ENDIF
          ISKN(1,JJ)=N1
          ISKN(2,JJ)=0
          ISKN(3,JJ)=0
          IF (INOD.EQ.1) THEN
C         defined with 3 nodes
C---      CALCUL DE X' et Y0'
            IF(N2D.EQ.0)THEN
              P(1)=X(1,N2)-X(1,N1)
              P(2)=X(2,N2)-X(2,N1)
              P(3)=X(3,N2)-X(3,N1)
              IF (N3.NE.0) THEN
                N3=USR2SYS(N3,ITABM1,MESSF,ID)
                CALL ANODSET(N3, CHECK_USED)
	        CALL IFRONTPLUS(N3,1)
              ENDIF
              P(4)=X(1,N3)-X(1,N1)
              P(5)=X(2,N3)-X(2,N1)
              P(6)=X(3,N3)-X(3,N1)
              P(10)=X(1,N1)
              P(11)=X(2,N1)
              P(12)=X(3,N1)
            ELSE
              P(1)=UN
              P(2)=ZERO
              P(3)=ZERO
              P(4)=X(1,N2)-X(1,N1)
              P(5)=X(2,N2)-X(2,N1)
              P(6)=X(3,N2)-X(3,N1)
              P(10)=X(1,N1)
              P(11)=X(2,N1)
              P(12)=X(3,N1)
            ENDIF
C---      TESTS DE CONSISTANCE
            PNOR1=SQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
            IF(PNOR1.LT.EM20) THEN
              CALL ANCMSG(MSGID=162,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I2=ITAB(N1),
     .                    I1=ID,C1=TITR,
     .                    I3=ITAB(N2))
              RETURN
            ENDIF
C---      CALCUL DE COLINEARITE DES VECTEURS N1N2 ET N1N3
            PNOR2=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
            IF(PNOR2.GT.EM20) THEN
              PNORM1=1./(PNOR1*PNOR2)
              DET1=ABS((P(1)*P(5)-P(2)*P(4))*PNORM1)
              DET2=ABS((P(1)*P(6)-P(3)*P(4))*PNORM1)
              DET3=ABS((P(2)*P(6)-P(3)*P(5))*PNORM1)
              DET= MAX(DET1,DET2,DET3)
            ELSE
              DET=ZERO
            ENDIF
            IF(DET.LT.EM5) THEN
              CALL ANCMSG(MSGID=163,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,C1=TITR)
              IF(ABS(P(2)).GT.EM5) THEN
                P(4)=ABS(P(1))+DIX
              ELSE
                P(5)=DIX
              ENDIF
            ENDIF
C---      CALCUL DE Z'
            P(7)=P(2)*P(6)-P(3)*P(5)
            P(8)=P(3)*P(4)-P(1)*P(6)
            P(9)=P(1)*P(5)-P(2)*P(4)
C---      CALCUL DE Y'
            P(4)=P(8)*P(3)-P(9)*P(2)
            P(5)=P(9)*P(1)-P(7)*P(3)
            P(6)=P(7)*P(2)-P(8)*P(1)
          ELSE
C         defined with 1 node and 2 vectors
C---      CALCUL DE X'
            P(10)=X(1,N1)
            P(11)=X(2,N1)
            P(12)=X(3,N1)
            P(1)=P(5)*P(9)-P(6)*P(8)
            P(2)=P(6)*P(7)-P(4)*P(9)
            P(3)=P(4)*P(8)-P(5)*P(7)
C---      CALCUL DE Y'
            P(4)=P(8)*P(3)-P(9)*P(2)
            P(5)=P(9)*P(1)-P(7)*P(3)
            P(6)=P(7)*P(2)-P(8)*P(1)
          ENDIF
        ELSE
C----------------
C     FRAME FIXE
C----------------
          ISKN(1,JJ)=0
          ISKN(2,JJ)=0
          ISKN(3,JJ)=0
          ISKN(5,JJ)=0
C-----------------
C     CALCUL DE X'
C-----------------
          P(1)=P(5)*P(9)-P(6)*P(8)
          P(2)=P(6)*P(7)-P(4)*P(9)
          P(3)=P(4)*P(8)-P(5)*P(7)
C-----------------
C     CALCUL DE Y'
C-----------------
          P(4)=P(8)*P(3)-P(9)*P(2)
          P(5)=P(9)*P(1)-P(7)*P(3)
          P(6)=P(7)*P(2)-P(8)*P(1)
          IF(SUB_ID .NE. 0)
     .      CALL SUBROTPOINT(P(10),P(11),P(12),RTRANS,SUB_ID,LSUBMODEL)
          IF(SUB_ID .NE. 0)
     .      CALL SUBROTVECT(P(1),P(2),P(3),RTRANS,SUB_ID,LSUBMODEL)
          IF(SUB_ID .NE. 0)
     .      CALL SUBROTVECT(P(4),P(5),P(6),RTRANS,SUB_ID,LSUBMODEL)
          IF(SUB_ID .NE. 0)
     .      CALL SUBROTVECT(P(7),P(8),P(9),RTRANS,SUB_ID,LSUBMODEL)
        ENDIF
C-----------
C     NORME
C-----------
        PP=SQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
        IF(PP.NE.ZERO)THEN
          P(1)=P(1)/PP
          P(2)=P(2)/PP
          P(3)=P(3)/PP
        ENDIF
        PP=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
        IF(PP.NE.ZERO)THEN
          P(4)=P(4)/PP
          P(5)=P(5)/PP
          P(6)=P(6)/PP
        ENDIF
        PP=SQRT(P(7)*P(7)+P(8)*P(8)+P(9)*P(9))
        IF(PP.NE.ZERO)THEN
          P(7)=P(7)/PP
          P(8)=P(8)/PP
          P(9)=P(9)/PP
        ENDIF
C
C-----------
        DO K=1,12
          XFRAME(K,I+1)=P(K)
        ENDDO
        DO K=1,9
          XFRAME(18+K,I+1)=P(K)
        ENDDO
C-----------
      ENDDO
C------------------------------------------------
      WRITE (IOUT,'(A)')'    REFERENCE FRAME SETS '
      WRITE (IOUT,'(A)')'    -------------------- '
      DO I=1,NUMFRAM
        J=I+1
        JJ=(NUMSKW+1)+MIN(IUN,NSPCOND)*NUMSPH+I+NSUBMOD+1
        NSK = ISKN(4,JJ)
C
        N1=ISKN(1,JJ)
        N2=ISKN(2,JJ)
        N3=ISKN(3,JJ)
        IF(N1.NE.0)N1=ITAB(N1)
        IF(N2.NE.0)N2=ITAB(N2)
        IF(N3.NE.0)N3=ITAB(N3)
        WRITE(IOUT,1000)
        WRITE(IOUT,'(1X,4I10,1X,3F16.7,3F16.7)')NSK,N1,N2,N3,
     &       (XFRAME(K,J),K=1,3),(XFRAME(K,J),K=10,12)
        WRITE(IOUT,'(2(42X,3F16.7/))') (XFRAME(K,J),K=4,9)
      ENDDO
C-----
 900  CONTINUE
C-------------------------------------
C Recherche des ID doubles
C-------------------------------------
      IF (NUMFRAM+NUMSKW.NE.0)
     .   CALL UDOUBLE(ISKN(4,1),LISKN,
     .        NUMSKW+1+MIN(IUN,NSPCOND)*NUMSPH+NUMFRAM+1+NSUBMOD,
     .        MESS,0,BID)
C-----
      RETURN
 999  CALL FREERR(1)

 1000 FORMAT(5X,'NUMBER',8X,'N1',8X,'N2',8X,'N3',10X,'VECTORS',42X,
     .      'ORIGIN')
 1001 FORMAT(5X,'NUMBER',10X,'VECTORS',42X,'ORIGIN')
      RETURN
      END

