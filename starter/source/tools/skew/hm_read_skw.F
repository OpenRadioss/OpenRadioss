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
Chd|  HM_READ_SKW                   source/tools/skew/hm_read_skw.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ANODSET                       source/output/analyse/analyse_node.c
Chd|        EULER_VROT                    source/model/submodel/euler_vrot.F
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        SUBROTPOINT                   source/model/submodel/subrot.F
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_SKW(SKEW    ,ISKN    ,X       ,ITAB     ,ITABM1  ,
     .                       NSN     ,LSUBMODEL,RTRANS ,NOM_OPT ,UNITAB)
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com10_c.inc"
#include      "sphcom.inc"
#include      "units_c.inc"
#include      "titr_c.inc"
#include      "param_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER ISKN(LISKN,*), ITAB(*), ITABM1(*), NSN(*)
C     REAL
      my_real
     .   SKEW(LSKEW,*), X(3,*),
     .   RTRANS(NTRANSF,*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, IMOV, J, N1, N2, N3, K, NSK,
     .        IUN, SUB_ID,
     .        IDSUB,ITY,L,READPT,J1,J2,NUMSPH_TMP,SUB_LEVEL,CUR_SUBMOD,
     .        IDIR,IFLAGUNIT,ID,UID,CPT
C     REAL
      my_real
     .   P(12), PNOR1, PNOR2, PNORM1, DET1, DET2, DET3, DET, PP,BID,
     .   X0(3),ROT(9)
      CHARACTER NOMSG*nchartitle
      CHARACTER TITR*nchartitle,MESS*40,KEY*ncharkey
      CHARACTER DIR*ncharfield
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
      DATA IUN/1/
      DATA MESS/'MOVING SKEW SYSTEM DEFINITION           '/
      DATA NOMSG/'global skew system                     '/
C=======================================================================
C
      DO I=1,LISKN
        ISKN(I,1)=0
      ENDDO
C
      DO I=1,LSKEW
        SKEW(I,1)=ZERO
      ENDDO
      SKEW(1,1)=ONE
      SKEW(5,1)=ONE
      SKEW(9,1)=ONE
      NOM_OPT(1,1)=0
      READPT=0
      CALL FRETITL(NOMSG,NOM_OPT(LNOPT1-LTITR+1,1),LTITR)
C
      IF(NUMSKW==0)GOTO 201
C--------------------------------------------------
C START BROWSING MODEL PROPERTIES
C--------------------------------------------------
      CALL HM_OPTION_START('/SKEW')
      I = 0
C--------------------------------------------------
C BROWSING MODEL PROPERTIES 1->HM_NUMGEO
C--------------------------------------------------
      DO 100 CPT=1,NUMSKW
        I = I + 1
C--------------------------------------------------
C EXTRACT DATAS OF /SKEW/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       UNIT_ID = UID,
     .                       SUBMODEL_ID = SUB_ID,
     .                       OPTION_TITR = TITR,
     .                       KEYWORD2 = KEY)

      NOM_OPT(1,I+1)=ID
      CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,I+1),LTITR)
      IMOV  = 0
C
      IFLAGUNIT = 0
      DO J=1,NUNITS
        IF (UNITAB%UNIT_ID(J) == UID) THEN                 
          IFLAGUNIT = 1
          EXIT
        ENDIF
      ENDDO
      IF (UID/=0.AND.IFLAGUNIT==0) THEN
        CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              I2=UID,I1=ID,C1='SKEW SYSTEM',C2='SKEW SYSTEM',
     .              C3=TITR)
      ENDIF
C
      IF(KEY(1:3)=='FIX')THEN
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

      ELSEIF (KEY(1:4)=='MOV2')THEN
        IMOV=2
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('originnodeid',N1,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('axisnodeid',N2,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('planenodeid',N3,IS_AVAILABLE,LSUBMODEL)

      ELSE
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
        ISKN(6,I+1)=IDIR
  
      ENDIF
      ISKN(4,I+1)=ID
C----------------
C     SKEW MOV2
C----------------
      IF (IMOV == 2) THEN
        N1=USR2SYS(N1,ITABM1,MESS,ID)
        N2=USR2SYS(N2,ITABM1,MESS,ID)
        N3=USR2SYS(N3,ITABM1,MESS,ID)
        CALL ANODSET(N1, CHECK_USED)
        CALL ANODSET(N2, CHECK_USED)
        CALL ANODSET(N3, CHECK_USED)
        ISKN(1,I+1)=N1
        ISKN(2,I+1)=N2
        ISKN(3,I+1)=N3
        ISKN(5,I+1)=IMOV
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
C       CALCUL DE L'ORIGINE
C-----------------
        P(10) = X(1,N1)
        P(11) = X(2,N1)
        P(12) = X(3,N1)
C----------------
C       TESTS DE CONSISTANCE
C----------------
        PNOR1=SQRT(P(7)*P(7)+P(8)*P(8)+P(9)*P(9))
        IF (PNOR1 < EM20) THEN
          CALL ANCMSG(MSGID=162,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I2=ITAB(N1),
     .                I1=N,C1=TITR,
     .                I3=ITAB(N2))
        ENDIF
C       CALCUL DE COLINEARITE DES VECTEURS N1N2 ET N1N3
        PNOR2=SQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
        IF (PNOR2 > EM20) THEN
          PNORM1=ONE/(PNOR1*PNOR2)
          DET1=ABS((P(8)*P(3)-P(9)*P(2))*PNORM1)
          DET2=ABS((P(9)*P(1)-P(7)*P(3))*PNORM1)
          DET3=ABS((P(7)*P(2)-P(8)*P(1))*PNORM1)
          DET= MAX(DET1,DET2,DET3)
        ELSE
          DET=ZERO
        ENDIF
        IF (DET < EM5) THEN
          CALL ANCMSG(MSGID=163,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,C1=TITR)
          IF(ABS(P(2)) < EM5) THEN
            P(4)=ABS(P(1))+TEN
          ELSE
            P(5)=TEN
          ENDIF
        ENDIF
C----------------
C     SKEW MOBILE (CALCUL DE LA POSITION INITIALE)
C----------------
      ELSEIF (IMOV==1) THEN
       N1=USR2SYS(N1,ITABM1,MESS,ID)
       N2=USR2SYS(N2,ITABM1,MESS,ID)
       CALL ANODSET(N1, CHECK_USED)
       CALL ANODSET(N2, CHECK_USED)
       ISKN(1,I+1)=N1
       ISKN(2,I+1)=N2
       ISKN(5,I+1)=IMOV
C-----------------
C     CALCUL DE X' et Y0' (IDIR=1) CALCUL DE Y' et Z0' (IDIR=2) CALCUL DE Z' et X0' (IDIR=3)
C-----------------
       IF(N2D==0)THEN
c
        IF (IDIR == 1) THEN
          P(1)=X(1,N2)-X(1,N1)
          P(2)=X(2,N2)-X(2,N1)
          P(3)=X(3,N2)-X(3,N1)
        ELSEIF (IDIR == 2) THEN
          P(4)=X(1,N2)-X(1,N1)
          P(5)=X(2,N2)-X(2,N1)
          P(6)=X(3,N2)-X(3,N1)
        ELSEIF (IDIR == 3) THEN
          P(7)=X(1,N2)-X(1,N1)
          P(8)=X(2,N2)-X(2,N1)
          P(9)=X(3,N2)-X(3,N1)
        ENDIF
c
        N3=USR2SYS(N3,ITABM1,MESS,ID)
        CALL ANODSET(N3, CHECK_USED)
        ISKN(3,I+1)=N3
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
c
       ELSE
        P(1)=ONE
        P(2)=ZERO
        P(3)=ZERO
C
        P(4)=X(1,N2)-X(1,N1)
        P(5)=X(2,N2)-X(2,N1)
        P(6)=X(3,N2)-X(3,N1)
       ENDIF

       P(10) = X(1,N1)
       P(11) = X(2,N1)
       P(12) = X(3,N1)
C----------------
C     TESTS DE CONSISTANCE
C----------------
       IF (IDIR == 1) PNOR1=SQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
       IF (IDIR == 2) PNOR1=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
       IF (IDIR == 3) PNOR1=SQRT(P(7)*P(7)+P(8)*P(8)+P(9)*P(9))
       IF(PNOR1<1.E-20) THEN
         CALL ANCMSG(MSGID=162,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I2=ITAB(N1),
     .               I1=ID,C1=TITR,
     .               I3=ITAB(N2))
       ENDIF
C     CALCUL DE COLINEARITE DES VECTEURS N1N2 ET N1N3
       IF (IDIR == 1) PNOR2=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
       IF (IDIR == 2) PNOR2=SQRT(P(7)*P(7)+P(8)*P(8)+P(9)*P(9))
       IF (IDIR == 3) PNOR2=SQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
       IF(PNOR2>1.E-20) THEN
         PNORM1=1./(PNOR1*PNOR2)
         IF (IDIR == 1) THEN
           DET1=ABS((P(1)*P(5)-P(2)*P(4))*PNORM1)
           DET2=ABS((P(1)*P(6)-P(3)*P(4))*PNORM1)
           DET3=ABS((P(2)*P(6)-P(3)*P(5))*PNORM1)
         ELSEIF (IDIR == 2) THEN
           DET1=ABS((P(4)*P(8)-P(5)*P(7))*PNORM1)
           DET2=ABS((P(4)*P(9)-P(6)*P(7))*PNORM1)
           DET3=ABS((P(5)*P(9)-P(6)*P(8))*PNORM1)
         ELSEIF (IDIR == 3) THEN
           DET1=ABS((P(7)*P(2)-P(8)*P(1))*PNORM1)
           DET2=ABS((P(7)*P(3)-P(9)*P(1))*PNORM1)
           DET3=ABS((P(8)*P(3)-P(9)*P(2))*PNORM1)
         ENDIF
         DET= MAX(DET1,DET2,DET3)
       ELSE
         DET=ZERO
       ENDIF
       IF(DET<EM5) THEN
         CALL ANCMSG(MSGID=163,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=ID,C1=TITR)
         IF (IDIR == 1) THEN
           IF(ABS(P(2))>EM5) THEN
             P(4)=ABS(P(1))+TEN
           ELSE
             P(5)=TEN
           ENDIF
         ELSEIF (IDIR == 2) THEN
           IF(ABS(P(5))>EM5) THEN
             P(7)=ABS(P(4))+TEN
           ELSE
             P(8)=TEN
           ENDIF
         ELSEIF (IDIR == 3) THEN
           IF(ABS(P(8))>EM5) THEN
             P(1)=ABS(P(7))+TEN
           ELSE
             P(2)=TEN
           ENDIF
         ENDIF
       ENDIF
C-----------------
C     CALCUL DE Z'(IDIR=1) X'(IDIR=2) Y'(IDIR=3)
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
C     CALCUL DE Y'(IDIR=1) Z'(IDIR=2) X'(IDIR=3) 
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
      ELSE
C----------------
C     SKEW FIXE
C----------------
C
       ISKN(1,I+1)=0
       ISKN(2,I+1)=0
       ISKN(3,I+1)=0
       ISKN(5,I+1)=0
C
       IF(P(4)==ZERO.AND.P(6)==ZERO) P(5)=SIGN(ONE,P(5))
       IF(P(7)==ZERO.AND.P(8)==ZERO) P(9)=SIGN(ONE,P(9))
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
C
       IF (SUB_ID /= 0)THEN
         X0(1:3) = ZERO
         DO J=1,NSUBMOD
           IF (LSUBMODEL(J)%NOSUBMOD == SUB_ID) IDSUB = J
         ENDDO
         CUR_SUBMOD = IDSUB
         SUB_LEVEL = LSUBMODEL(IDSUB)%LEVEL
         DO WHILE (SUB_LEVEL /= 0)
           DO J=1,LSUBMODEL(CUR_SUBMOD)%NBTRANS
             ITY = RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(J),2)
             IF( ITY == 2 .OR. ITY == 3 ) THEN
               DO K=1,9
                 ROT(K) = RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(J),K+2)
               ENDDO
               CALL EULER_VROT(X0,P(1),ROT)
               CALL EULER_VROT(X0,P(4),ROT)
               CALL EULER_VROT(X0,P(7),ROT)
             ENDIF
           ENDDO
           SUB_LEVEL = SUB_LEVEL - 1
           CUR_SUBMOD = LSUBMODEL(CUR_SUBMOD)%IFATHER
         ENDDO
         IF(LSUBMODEL(IDSUB)%NBTRANS /=0)
     .      CALL SUBROTPOINT(P(10),P(11),P(12),RTRANS,SUB_ID,LSUBMODEL)
       ENDIF
      ENDIF
C-----------
C     NORME
C-----------
      PP=SQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
      P(1)=P(1)/PP
      P(2)=P(2)/PP
      P(3)=P(3)/PP
      PP=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
      P(4)=P(4)/PP
      P(5)=P(5)/PP
      P(6)=P(6)/PP
      PP=SQRT(P(7)*P(7)+P(8)*P(8)+P(9)*P(9))
      P(7)=P(7)/PP
      P(8)=P(8)/PP
      P(9)=P(9)/PP
C
      DO K=1,12
        SKEW(K,I+1)=P(K)
      ENDDO
C
  100 CONTINUE
C
      WRITE (IOUT,'(A)')TITRE(85)
      WRITE (IOUT,'(A)')TITRE(81)
      DO 110 I=1,NUMSKW
      J=I+1
      NSK = ISKN(4,J)
      N1=ISKN(1,J)
      N2=ISKN(2,J)
      N3=ISKN(3,J)
      IF(N1/=0)THEN
       N1=ITAB(N1)
       N2=ITAB(N2)
       N3=ITAB(N3)
      ENDIF
      WRITE(IOUT,1000)
      WRITE(IOUT,'(1X,4I10,1X,3F16.7,3F16.7)')NSK,N1,N2,N3,
     &                (SKEW(K,J),K=1,3),(SKEW(K,J),K=10,12)
      WRITE(IOUT,'(3(42X,3F16.7/))')  (SKEW(K,J),K=4,9)
  110 CONTINUE
C
      DO 140 K = 1,NSNOD
          NSN(K) = IABS(NSN(K))
 140  CONTINUE
C
 201  CONTINUE
C-------------------------------------------------------------
C   building SKEWS for SPH KINEMATIC CONDITIONS ADDITION.
C-------------------------------------------------------------
      IF(NSPCOND/=0)THEN
        DO J=(NUMSKW+1)+1,(NUMSKW+1)+NUMSPH
             DO K=1,LSKEW
              SKEW(K,J)=ZERO
             ENDDO
             SKEW(1,J)=ONE
             SKEW(5,J)=ONE
             SKEW(9,J)=ONE
             ISKN(1,J)=0
             ISKN(2,J)=0
             ISKN(3,J)=0
C          not a user's defined skew.
             ISKN(4,J)=-(J-NUMSKW)
        ENDDO
      ENDIF
C-------------------------------------------------------------
Csv begin   building SKEWS for SUBMODEL (Rotation Transformation)
C-------------------------------------------------------------
      IF(NSUBMOD/=0)THEN
        WRITE (IOUT,'(A)')TITRE(118)
        WRITE (IOUT,'(A)')TITRE(119)
        WRITE (IOUT,1001)
        IF (NSPCOND/=0) THEN
          J1 = (NUMSKW+1)+NUMSPH+1
          J2 = (NUMSKW+1)+NUMSPH+NSUBMOD
          NUMSPH_TMP = NUMSPH
        ELSE
          NUMSPH_TMP = 0
          J1 = (NUMSKW+1)+1
          J2 = (NUMSKW+1)+NSUBMOD
        ENDIF
        DO J=J1,J2
          DO K=1,LSKEW
           SKEW(K,J)=ZERO
          ENDDO
          SKEW(1,J)=ONE
          SKEW(5,J)=ONE
          SKEW(9,J)=ONE
          ISKN(1,J)=0
          ISKN(2,J)=0
          ISKN(3,J)=0
          ISKN(5,J)=0
          IDSUB = J-(NUMSKW+NUMSPH_TMP+1)
          CUR_SUBMOD = IDSUB
          SUB_LEVEL = LSUBMODEL(IDSUB)%LEVEL
          X0(1:3) = ZERO

          DO WHILE (SUB_LEVEL /= 0)
            DO K=1,LSUBMODEL(CUR_SUBMOD)%NBTRANS
              ITY = RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(K),2)
              IF( ITY == 2 .OR. ITY == 3 )THEN
                DO L=1,9
                  ROT(L) = RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(K),L+2)
                ENDDO
                CALL EULER_VROT(X0,SKEW(1,J),ROT)
                CALL EULER_VROT(X0,SKEW(4,J),ROT)
                CALL EULER_VROT(X0,SKEW(7,J),ROT)
              ENDIF
            ENDDO
            SUB_LEVEL = SUB_LEVEL - 1
            CUR_SUBMOD = LSUBMODEL(CUR_SUBMOD)%IFATHER
          ENDDO
          IF(LSUBMODEL(IDSUB)%NBTRANS /=0)
     .      CALL SUBROTPOINT(SKEW(10,J),SKEW(11,J),SKEW(12,J),
     .        RTRANS,LSUBMODEL(IDSUB)%NOSUBMOD,LSUBMODEL)
Csv          not a user's defined skew.
             ISKN(4,J)=1000000001 + (J-NUMSKW-NUMSPH_TMP-2)
          WRITE(IOUT,'(1X,I10,1X,3F16.7,3F16.7)')ISKN(4,J),
     .                (SKEW(K,J),K=1,3),(SKEW(K,J),K=10,12)
          WRITE(IOUT,'(3(12X,3F16.7/))')  (SKEW(K,J),K=4,9)
        ENDDO
      ENDIF
C
C-------------------------------------
C Recherche des ID doubles
C-------------------------------------
      IF (NUMSKW/=0)
     .   CALL UDOUBLE(ISKN(4,1),LISKN,
     .        NUMSKW+1+MIN(IUN,NSPCOND)*NUMSPH+NSUBMOD,
     .        MESS,0,BID)
C-----
      RETURN
C-----
 1000 FORMAT(5X,'NUMBER',8X,'N1',8X,'N2',8X,'N3',10X,'VECTORS',42X,
     .      'ORIGIN')
 1001 FORMAT(5X,'NUMBER',10X,'VECTORS',42X,'ORIGIN')
C-----
      RETURN
      END
