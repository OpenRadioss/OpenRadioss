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
!||    lgmini_fxv             ../starter/source/tools/lagmul/lgmini_fxv.F
!||--- called by ------------------------------------------------------
!||    lagm_ini               ../starter/source/tools/lagmul/lagm_ini.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    fretitl2               ../starter/source/starter/freform.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE LGMINI_FXV(IADLL  ,JLL    ,LLL    ,IBFV   ,VEL    ,
     .                      NC, MASS, INER, ITAB   ,NOM_OPT)
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C----------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "lagmult.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC, IADLL(*), JLL(*), LLL(*), IBFV(NIFV,*),
     .        ITAB(*)
      my_real
     .        VEL(LFXVELR,*), MASS(*), INER(*)
      INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, IK, ISK, NNO
      INTEGER ID
      CHARACTER(LEN=NCHARTITLE) :: TITR
C======================================================================| 
      DO N=1,NFXVEL
        ID=NOM_OPT(1,IBFV(12,N))
        CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,IBFV(12,N)),LTITR)
        IF (IBFV(8,N)/=0) THEN
          NNO = IABS(IBFV(1,N))
          ISK = IBFV(2,N)/10
          J=IBFV(2,N)-10*ISK
          NC = NC + 1
          IF(J<=3.AND.MASS(NNO)==0)THEN
            CALL ANCMSG(MSGID=535,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  C1='FIXED TRANSLATIONAL VELOCITY',
     .                  I1=ID,
     .                  C2='FIXED TRANSLATIONAL VELOCITY',
     .                  C3=TITR,C4=' ',
     .                  I2=ITAB(IABS(IBFV(1,N))))
            ELSEIF(J>3.AND.MASS(NNO)==0)THEN
            CALL ANCMSG(MSGID=535,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  C1='FIXED ROTATIONAL VELOCITY',
     .                  I1=ID,
     .                  C2='FIXED ROTATIONAL VELOCITY',
     .                  C3=TITR,C4=' ',
     .                  I2=ITAB(IABS(IBFV(1,N))))
            ENDIF
Cm41u07---
            IF(NC>LAG_NCF)THEN
            CALL ANCMSG(MSGID=468,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1='IMPOSED VELOCITY',
     .                  C2='IMPOSED VELOCITY',
     .                  C3=TITR)
          ENDIF
          IF (ISK<=1) THEN
            IADLL(NC+1)=IADLL(NC) + 1
            IF(IADLL(NC+1)-1>LAG_NKF)THEN
              CALL ANCMSG(MSGID=469,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    C1='IMPOSED VELOCITY',
     .                    I1=ID,
     .                    C2='IMPOSED VELOCITY',
     .                    C3=TITR)
            ENDIF
            IK = IADLL(NC)
            LLL(IK) = NNO
            JLL(IK) = J
            IF(J>3.AND.INER(NNO)==0)THEN
               CALL ANCMSG(MSGID=536,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO_BLIND_1,
     .                     C1='FIXED ROTATIONAL VELOCITY',
     .                     I1=ID,
     .                     C2='FIXED ROTATIONAL VELOCITY',
     .                     C3=TITR,
     .                     I2=ITAB(IABS(IBFV(1,N))))
              ENDIF
          ELSE
            IADLL(NC+1)=IADLL(NC) + 3
            IF(IADLL(NC+1)-1>LAG_NKF)THEN
              CALL ANCMSG(MSGID=469,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1='IMPOSED VELOCITY',
     .                    C2='IMPOSED VELOCITY',
     .                    C3=TITR)
            ENDIF
            IF(J<=3)THEN
              IK = IADLL(NC)
              LLL(IK) = NNO
              JLL(IK) = 1
              IK = IK + 1
              LLL(IK) = NNO
              JLL(IK) = 2
              IK = IK + 1
              LLL(IK) = NNO
              JLL(IK) = 3
            ELSE
              IF(INER(NNO)==0)THEN
               CALL ANCMSG(MSGID=536,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO_BLIND_1,
     .                     C1='FIXED ROTATIONAL VELOCITY',
     .                     I1=ID,
     .                     C2='FIXED ROTATIONAL VELOCITY',
     .                     C3=TITR,
     .                     I2=ITAB(IABS(IBFV(1,N))))
                ENDIF
              IK = IADLL(NC)
              LLL(IK) = NNO
              JLL(IK) = 4
              IK = IK + 1
              LLL(IK) = NNO
              JLL(IK) = 5
              IK = IK + 1
              LLL(IK) = NNO
              JLL(IK) = 6
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C---
      RETURN
      END
