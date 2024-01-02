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
Chd|====================================================================
Chd|  LGMINI_BC                     source/tools/lagmul/lgmini_bc.F
Chd|-- called by -----------
Chd|        LAGM_INI                      source/tools/lagmul/lagm_ini.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LGMINI_BC(IADLL  ,JLL   ,LLL   ,IGRNOD ,IBCSLAG,
     .                     MASS   ,INER  ,NC    ,NOM_OPT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
C----------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "lagmult.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC, IADLL(*), JLL(*), LLL(*),IBCSLAG(5,*)
      my_real
     .        MASS(*),INER(*)
      INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IK, IG, IGR, CT, CR, ISK, NN
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C======================================================================|
      DO I=1,NBCSLAG
        IGR = IBCSLAG(1,I)
        CT  = IBCSLAG(2,I)
        CR  = IBCSLAG(3,I)
        ISK = IBCSLAG(4,I)
        ID=NOM_OPT(1,I)
        CALL FRETITL2(TITR,
     .       NOM_OPT(LNOPT1-LTITR+1,I),LTITR)
        DO IG=1,IGRNOD(IGR)%NENTITY
          NN=IGRNOD(IGR)%ENTITY(IG)
          IF(MASS(NN)/=0.0)THEN
C--- Translations
            IF(CT==1.OR.CT==3.OR.CT==5.OR.CT==7)THEN
C--- dz
              NC = NC + 1
              IF(NC>LAG_NCF)THEN
                CALL ANCMSG(MSGID=500,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      C1='BCS',
     .                      I1=ID,
     .                      C2='BCS',
     .                      C3=TITR)
              ENDIF
              IF(ISK==1)THEN
                IADLL(NC+1)=IADLL(NC) + 1
                IF(IADLL(NC+1)-1>LAG_NKF)THEN
                  CALL ANCMSG(MSGID=469,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO,
     .                        I1=ID,
     .                        C1='BOUNDARY CONDITIONS',
     .                        C2='BOUNDARY CONDITIONS',
     .                        C3=TITR)
                ENDIF
                IK = IADLL(NC)
                LLL(IK) = NN
                JLL(IK) = 3
              ELSE
                IADLL(NC+1)=IADLL(NC) + 3
                IF(IADLL(NC+1)-1>LAG_NKF)THEN
                  CALL ANCMSG(MSGID=469,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO,
     .                        I1=ID,
     .                        C1='BOUNDARY CONDITIONS',
     .                        C2='BOUNDARY CONDITIONS',
     .                        C3=TITR)
                ENDIF
                IK = IADLL(NC)
                LLL(IK) = NN
                JLL(IK) = 1
                IK = IK + 1
                LLL(IK) = NN
                JLL(IK) = 2
                IK = IK + 1
                LLL(IK) = NN
                JLL(IK) = 3
              ENDIF
            ENDIF
            IF(CT==2.OR.CT==3.OR.CT==6.OR.CT==7)THEN
C--- dy
              NC = NC + 1
              IF(NC>LAG_NCF)THEN
                CALL ANCMSG(MSGID=468,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      I1=ID,
     .                      C1='BOUNDARY CONDITIONS',
     .                      C2='BOUNDARY CONDITIONS',
     .                      C3=TITR)
              ENDIF
              IF(ISK==1)THEN
                IADLL(NC+1)=IADLL(NC) + 1
                IF(IADLL(NC+1)-1>LAG_NKF)THEN
                  CALL ANCMSG(MSGID=469,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO,
     .                        I1=ID,
     .                        C1='BOUNDARY CONDITIONS',
     .                        C2='BOUNDARY CONDITIONS',
     .                        C3=TITR)
                ENDIF
                IK = IADLL(NC)
                LLL(IK) = NN
                JLL(IK) = 2
              ELSE
                IADLL(NC+1)=IADLL(NC) + 3
                IF(IADLL(NC+1)-1>LAG_NKF)THEN
                  CALL ANCMSG(MSGID=469,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO,
     .                        I1=ID,
     .                        C1='BOUNDARY CONDITIONS',
     .                        C2='BOUNDARY CONDITIONS',
     .                        C3=TITR)
                ENDIF
                IK = IADLL(NC)
                LLL(IK) = NN
                JLL(IK) = 1
                IK = IK + 1
                LLL(IK) = NN
                JLL(IK) = 2
                IK = IK + 1
                LLL(IK) = NN
                JLL(IK) = 3
              ENDIF
            ENDIF
            IF(CT==4.OR.CT==5.OR.CT==6.OR.CT==7)THEN
C--- dx
              NC = NC + 1
              IF(NC>LAG_NCF)THEN
                CALL ANCMSG(MSGID=468,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      I1=ID,
     .                      C1='BOUNDARY CONDITIONS',
     .                      C2='BOUNDARY CONDITIONS',
     .                      C3=TITR)
              ENDIF
              IF(ISK==1)THEN
                IADLL(NC+1)=IADLL(NC) + 1
                IF(IADLL(NC+1)-1>LAG_NKF)THEN
                  CALL ANCMSG(MSGID=469,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO,
     .                        I1=ID,
     .                        C1='BOUNDARY CONDITIONS',
     .                        C2='BOUNDARY CONDITIONS',
     .                        C3=TITR)
                ENDIF
                IK = IADLL(NC)
                LLL(IK) = NN
                JLL(IK) = 1
              ELSE
                IADLL(NC+1)=IADLL(NC) + 3
                IF(IADLL(NC+1)-1>LAG_NKF)THEN
                  CALL ANCMSG(MSGID=469,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO,
     .                        I1=ID,
     .                        C1='BOUNDARY CONDITIONS',
     .                        C2='BOUNDARY CONDITIONS',
     .                        C3=TITR)
                ENDIF
                IK = IADLL(NC)
                LLL(IK) = NN
                JLL(IK) = 1
                IK = IK + 1
                LLL(IK) = NN
                JLL(IK) = 2
                IK = IK + 1
                LLL(IK) = NN
                JLL(IK) = 3
              ENDIF
            ENDIF            
          ENDIF
          IF(INER(NN)/=0.0)THEN
C--- Rotations
            IF(CR==1.OR.CR==3.OR.CR==5.OR.CR==7)THEN
C--- rz
              NC = NC + 1
              IF(NC>LAG_NCF)THEN
                CALL ANCMSG(MSGID=468,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      I1=ID,
     .                      C1='BOUNDARY CONDITIONS')
              ENDIF
              IF(ISK==1)THEN
                IADLL(NC+1)=IADLL(NC) + 1
                IF(IADLL(NC+1)-1>LAG_NKF)THEN
                  CALL ANCMSG(MSGID=469,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO,
     .                        I1=ID,
     .                        C1='BOUNDARY CONDITIONS',
     .                        C2='BOUNDARY CONDITIONS',
     .                        C3=TITR)
                ENDIF
                IK = IADLL(NC)
                LLL(IK) = NN
                JLL(IK) = 6
              ELSE
                IADLL(NC+1)=IADLL(NC) + 3
                IF(IADLL(NC+1)-1>LAG_NKF)THEN
                  CALL ANCMSG(MSGID=469,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO,
     .                        I1=ID,
     .                        C1='BOUNDARY CONDITIONS',
     .                        C2='BOUNDARY CONDITIONS',
     .                        C3=TITR)
                ENDIF
                IK = IADLL(NC)
                LLL(IK) = NN
                JLL(IK) = 4
                IK = IK + 1
                LLL(IK) = NN
                JLL(IK) = 5
                IK = IK + 1
                LLL(IK) = NN
                JLL(IK) = 6
              ENDIF
            ENDIF
            IF(CR==2.OR.CR==3.OR.CR==6.OR.CR==7)THEN
C--- ry
              NC = NC + 1
              IF(NC>LAG_NCF)THEN
                CALL ANCMSG(MSGID=468,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      I1=ID,
     .                      C1='BOUNDARY CONDITIONS',
     .                      C2='BOUNDARY CONDITIONS',
     .                      C3=TITR)
              ENDIF
              IF(ISK==1)THEN
                IADLL(NC+1)=IADLL(NC) + 1
                IF(IADLL(NC+1)-1>LAG_NKF)THEN
                  CALL ANCMSG(MSGID=469,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO,
     .                        I1=ID,
     .                        C1='BOUNDARY CONDITIONS',
     .                        C2='BOUNDARY CONDITIONS',
     .                        C3=TITR)
                ENDIF
                IK = IADLL(NC)
                LLL(IK) = NN
                JLL(IK) = 5
              ELSE
                IADLL(NC+1)=IADLL(NC) + 3
                IF(IADLL(NC+1)-1>LAG_NKF)THEN
                  CALL ANCMSG(MSGID=469,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO,
     .                        I1=ID,
     .                        C1='BOUNDARY CONDITIONS',
     .                        C2='BOUNDARY CONDITIONS',
     .                        C3=TITR)
                ENDIF
                IK = IADLL(NC)
                LLL(IK) = NN
                JLL(IK) = 4
                IK = IK + 1
                LLL(IK) = NN
                JLL(IK) = 5
                IK = IK + 1
                LLL(IK) = NN
                JLL(IK) = 6
              ENDIF
            ENDIF
            IF(CR==4.OR.CR==5.OR.CR==6.OR.CR==7)THEN
C--- rx
              NC = NC + 1
              IF(NC>LAG_NCF)THEN
                CALL ANCMSG(MSGID=468,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      I1=ID,
     .                      C1='BOUNDARY CONDITIONS',
     .                      C2='BOUNDARY CONDITIONS',
     .                      C3=TITR)
              ENDIF
              IF(ISK==1)THEN
                IADLL(NC+1)=IADLL(NC) + 1
                IF(IADLL(NC+1)-1>LAG_NKF)THEN
                  CALL ANCMSG(MSGID=469,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO,
     .                        I1=ID,
     .                        C1='BOUNDARY CONDITIONS',
     .                        C2='BOUNDARY CONDITIONS',
     .                        C3=TITR)
                ENDIF
                IK = IADLL(NC)
                LLL(IK) = NN
                JLL(IK) = 4
              ELSE
                IADLL(NC+1)=IADLL(NC) + 3
                IF(IADLL(NC+1)-1>LAG_NKF)THEN
                  CALL ANCMSG(MSGID=469,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO,
     .                        I1=ID,
     .                        C1='BOUNDARY CONDITIONS',
     .                        C2='BOUNDARY CONDITIONS',
     .                        C3=TITR)
                ENDIF
                IK = IADLL(NC)
                LLL(IK) = NN
                JLL(IK) = 4
                IK = IK + 1
                LLL(IK) = NN
                JLL(IK) = 5
                IK = IK + 1
                LLL(IK) = NN
                JLL(IK) = 6
              ENDIF
            ENDIF
          ENDIF
C---
        ENDDO
      ENDDO
C---
      RETURN
      END
