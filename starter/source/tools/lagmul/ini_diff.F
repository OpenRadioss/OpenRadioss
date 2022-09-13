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
Chd|  INI_DIFF                      source/tools/lagmul/ini_diff.F
Chd|-- called by -----------
Chd|        LGMINI_GJ                     source/tools/lagmul/lgmini_gj.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INI_DIFF(IADLL ,JLL   ,LLL   ,N0    ,N1    ,
     .                    N2    ,N3    ,NC,ID,TITR)
      USE MESSAGE_MOD
C----------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "lagmult.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N0, N1, N2, N3 ,NC, IADLL(*), JLL(*), LLL(*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, J, JJ, IK, IAD, INOD(4)
C======================================================================|
      INOD(1) = N1
      INOD(2) = N2
      INOD(3) = N3
      INOD(4) = N0
C--- vx
      NC = NC + 1
      IF(NC>LAG_NCF)THEN
        CALL ANCMSG(MSGID=468,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1='DIFFERENTIAL GEAR',
     .              C2='DIFFERENTIAL GEAR',
     .              C3=TITR)
      ENDIF
      IADLL(NC+1) = IADLL(NC) + 10
      IF(IADLL(NC+1)-1>LAG_NKF)THEN
        CALL ANCMSG(MSGID=469,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1='DIFFERENTIAL GEAR',
     .              C2='DIFFERENTIAL GEAR',
     .              C3=TITR)
      ENDIF
      IAD = IADLL(NC) -1
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 1
      ENDDO
      IAD = IAD + 3
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 2
      ENDDO
      IAD = IAD + 3
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 3
      ENDDO
      IK = IAD + 4
      LLL(IK) = INOD(4)
      JLL(IK) = 1
C--- vy
      NC = NC + 1
      IF(NC>LAG_NCF)THEN
        CALL ANCMSG(MSGID=468,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1='DIFFERENTIAL GEAR',
     .              C2='DIFFERENTIAL GEAR',
     .              C3=TITR)
      ENDIF
      IADLL(NC+1) = IADLL(NC) + 10
      IF(IADLL(NC+1)-1>LAG_NKF)THEN
        CALL ANCMSG(MSGID=469,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1='DIFFERENTIAL GEAR',
     .              C2='DIFFERENTIAL GEAR',
     .              C3=TITR)
      ENDIF
      IAD = IADLL(NC) -1
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 1
      ENDDO
      IAD = IAD + 3
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 2
      ENDDO
      IAD = IAD + 3
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 3
      ENDDO
      IK = IAD + 4
      LLL(IK) = INOD(4)
      JLL(IK) = 2
C--- vz
      NC = NC + 1
      IF(NC>LAG_NCF)THEN
        CALL ANCMSG(MSGID=468,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1='DIFFERENTIAL GEAR',
     .              C2='DIFFERENTIAL GEAR',
     .              C3=TITR)
      ENDIF
      IADLL(NC+1) = IADLL(NC) + 10
      IF(IADLL(NC+1)-1>LAG_NKF)THEN
        CALL ANCMSG(MSGID=469,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1='DIFFERENTIAL GEAR',
     .              C2='DIFFERENTIAL GEAR',
     .              C3=TITR)
      ENDIF
      IAD = IADLL(NC) -1
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 1
      ENDDO
      IAD = IAD + 3
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 2
      ENDDO
      IAD = IAD + 3
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 3
      ENDDO
      IK = IAD + 4
      LLL(IK) = INOD(4)
      JLL(IK) = 3
C --- wx
      NC = NC + 1
      IF(NC>LAG_NCF)THEN
        CALL ANCMSG(MSGID=468,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1='DIFFERENTIAL GEAR',
     .              C2='DIFFERENTIAL GEAR',
     .              C3=TITR)
      ENDIF
      IADLL(NC+1) = IADLL(NC) + 10
      IF(IADLL(NC+1)-1>LAG_NKF)THEN
        CALL ANCMSG(MSGID=469,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1='DIFFERENTIAL GEAR',
     .              C2='DIFFERENTIAL GEAR',
     .              C3=TITR)
      ENDIF
      IAD = IADLL(NC) -1
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 1
      ENDDO
      IAD = IAD + 3
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 2
      ENDDO
      IAD = IAD + 3
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 3
      ENDDO
      IK = IAD + 4
      LLL(IK) = INOD(4)
      JLL(IK) = 4
C --- wy
      NC = NC + 1
      IF(NC>LAG_NCF)THEN
        CALL ANCMSG(MSGID=468,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1='DIFFERENTIAL GEAR',
     .              C2='DIFFERENTIAL GEAR',
     .              C3=TITR)
      ENDIF
      IADLL(NC+1) = IADLL(NC) + 10
      IF(IADLL(NC+1)-1>LAG_NKF)THEN
        CALL ANCMSG(MSGID=469,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1='DIFFERENTIAL GEAR',
     .              C2='DIFFERENTIAL GEAR',
     .              C3=TITR)
      ENDIF
      IAD = IADLL(NC) -1
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 1
      ENDDO
      IAD = IAD + 3
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 2
      ENDDO
      IAD = IAD + 3
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 3
      ENDDO
      IK = IAD + 4
      LLL(IK) = INOD(4)
      JLL(IK) = 5
C --- wz
      NC = NC + 1
      IF(NC>LAG_NCF)THEN
        CALL ANCMSG(MSGID=468,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1='DIFFERENTIAL GEAR',
     .              C2='DIFFERENTIAL GEAR',
     .              C3=TITR)
      ENDIF
      IADLL(NC+1) = IADLL(NC) + 10
      IF(IADLL(NC+1)-1>LAG_NKF)THEN
        CALL ANCMSG(MSGID=469,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1='DIFFERENTIAL GEAR',
     .              C2='DIFFERENTIAL GEAR',
     .              C3=TITR)
      ENDIF
      IAD = IADLL(NC) -1
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 1
      ENDDO
      IAD = IAD + 3
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 2
      ENDDO
      IAD = IAD + 3
      DO JJ=1,3
        IK = IAD+JJ
        LLL(IK) = INOD(JJ)
        JLL(IK) = 3
      ENDDO
      IK = IAD + 4
      LLL(IK) = INOD(4)
      JLL(IK) = 6
C
C     Local Constraints
C --- local X
      INOD(4) = N0
      NC = NC + 1
      IF(NC>LAG_NCF)THEN
        CALL ANCMSG(MSGID=468,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1='DIFFERENTIAL GEAR',
     .              C2='DIFFERENTIAL GEAR',
     .              C3=TITR)
      ENDIF
      IADLL(NC+1)=IADLL(NC) + 12
      IF(IADLL(NC+1)-1>LAG_NKF)THEN
        CALL ANCMSG(MSGID=469,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1='DIFFERENTIAL GEAR',
     .              C2='DIFFERENTIAL GEAR',
     .              C3=TITR)
      ENDIF
      IK = IADLL(NC)
      LLL(IK) = N1
      JLL(IK) = 4
      IK = IK+1
      LLL(IK) = N1
      JLL(IK) = 5
      IK = IK+1
      LLL(IK) = N1
      JLL(IK) = 6
      DO I=2,3
        IK = IK+1
        LLL(IK) = INOD(I)
        JLL(IK) = 4
        IK = IK+1
        LLL(IK) = INOD(I)
        JLL(IK) = 5
        IK = IK+1
        LLL(IK) = INOD(I)
        JLL(IK) = 6
      ENDDO
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 4
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 5
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 6
C
C     local Y 
      DO I=1,3
        NC = NC + 1
        IF(NC>LAG_NCF)THEN
          CALL ANCMSG(MSGID=468,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .              C1='DIFFERENTIAL GEAR',
     .              C2='DIFFERENTIAL GEAR',
     .              C3=TITR)
        ENDIF
        IADLL(NC+1)=IADLL(NC) + 6
        IF(IADLL(NC+1)-1>LAG_NKF)THEN
          CALL ANCMSG(MSGID=469,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1='DIFFERENTIAL GEAR',
     .                C2='DIFFERENTIAL GEAR',
     .                C3=TITR)
        ENDIF
        IK = IADLL(NC)
        LLL(IK) = INOD(I)
        JLL(IK) = 4
        IK = IK+1
        LLL(IK) = INOD(I)
        JLL(IK) = 5
        IK = IK+1
        LLL(IK) = INOD(I)
        JLL(IK) = 6
        IK = IK+1
        LLL(IK) = N0
        JLL(IK) = 4
        IK = IK+1
        LLL(IK) = N0
        JLL(IK) = 5
        IK = IK+1
        LLL(IK) = N0
        JLL(IK) = 6
      ENDDO
C
C     local Z
      DO I=1,3
        NC = NC + 1
        IF(NC>LAG_NCF)THEN
          CALL ANCMSG(MSGID=468,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1='DIFFERENTIAL GEAR',
     .                C2='DIFFERENTIAL GEAR',
     .                C3=TITR)
        ENDIF
        IADLL(NC+1)=IADLL(NC) + 6
        IF(IADLL(NC+1)-1>LAG_NKF)THEN
          CALL ANCMSG(MSGID=469,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1='DIFFERENTIAL GEAR',
     .                C2='DIFFERENTIAL GEAR',
     .                C3=TITR)
        ENDIF
        IK = IADLL(NC)
        LLL(IK) = INOD(I)
        JLL(IK) = 4
        IK = IK+1
        LLL(IK) = INOD(I)
        JLL(IK) = 5
        IK = IK+1
        LLL(IK) = INOD(I)
        JLL(IK) = 6
        IK = IK+1
        LLL(IK) = N0
        JLL(IK) = 4
        IK = IK+1
        LLL(IK) = N0
        JLL(IK) = 5
        IK = IK+1
        LLL(IK) = N0
        JLL(IK) = 6
      ENDDO
C---
      RETURN
      END
