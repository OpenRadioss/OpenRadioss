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
Chd|  LGMINI_I2                     source/tools/lagmul/lgmini_i2.F
Chd|-- called by -----------
Chd|        LAGM_INI                      source/tools/lagmul/lagm_ini.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LGMINI_I2(IADLL ,JLL  ,LLL  ,INTBUF_TAB ,IPARI, NC,
     .            MASS , ITAB    ,NOM_OPT) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD 
      USE INTBUFDEF_MOD
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
#include      "param_c.inc"
#include      "units_c.inc"
#include      "lagmult.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC, IADLL(*), JLL(*), LLL(*),
     .        IPARI(NPARI,NINTER),ITAB(*)
      my_real
     .        MASS(*) 
      INTEGER NOM_OPT(LNOPT1,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER INOD(5), N,  II, JJ, IK, L, 
     .        ILAGM, NRTS, NRTM, NSN, NMN,IRECT ,NSV ,IRTL, IADDB,
     .        NIR, NTY, NDL, IAD, I, J, NN0, NM0
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C======================================================================|
      DO N=1,NINTER
        ID=NOM_OPT(1,N)
        CALL FRETITL2(TITR,
     .       NOM_OPT(LNOPT1-LTITR+1,N),LTITR)
        NTY   = IPARI(7 ,N)
        ILAGM = IPARI(33,N)
        IF (NTY == 2 .AND. ILAGM == 1)THEN
          NRTS  =IPARI(3,N)
          NRTM  =IPARI(4,N)
          NSN   =IPARI(5,N)
          NMN   =IPARI(6,N)
C---
          DO I=1,NSN
            NN0=INTBUF_TAB(N)%NSV(I)
            IF(MASS(NN0) == 0)THEN
              CALL ANCMSG(MSGID=535,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    C1='INTERFACE TYPE2',
     .                    I1=ID,
     .                    C2='INTERFACE TYPE2',
     .                    C3=TITR,C4='SECONDARY',
     .                    I2=ITAB(NN0))
            ENDIF  
              ENDDO
              DO J=1,NMN
                NM0=INTBUF_TAB(N)%MSR(J)
                IF(MASS(NM0) == 0)THEN
              CALL ANCMSG(MSGID=535,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    C1='INTERFACE',
     .                    I1=ID,
     .                    C2='INTERFACE',
     .                    C3=TITR,C4='SECONDARY',
     .                    I2=ITAB(NM0))
                ENDIF  
              ENDDO
C
          DO II=1,NSN
            L = INTBUF_TAB(N)%IRTLM(II)
            NIR = 4
            DO JJ=1,NIR
              INOD(JJ) = INTBUF_TAB(N)%IRECTM((L-1)*NIR+JJ)
            ENDDO
            IF(INOD(4) == INOD(3))  NIR=3
            INOD(NIR+1)=INTBUF_TAB(N)%NSV(II)
            NDL  = 3*NIR+1
C ---  ic = 1 (vx)
            NC = NC + 1
            IF(NC > LAG_NCF)THEN
              CALL ANCMSG(MSGID=468,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1='INTERFACE TYPE 2',
     .                    C2='INTERFACE TYPE 2',
     .                    C3=TITR)
            ENDIF
            IADLL(NC+1) = IADLL(NC) + NDL
            IF(IADLL(NC+1)-1 > LAG_NKF)THEN
              CALL ANCMSG(MSGID=469,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1='INTERFACE TYPE 2',
     .                    C2='INTERFACE TYPE 2',
     .                    C3=TITR)
            ENDIF
            IAD = IADLL(NC) -1
            DO JJ=1,NIR
              IK = IAD+JJ
              LLL(IK) = INOD(JJ)
              JLL(IK) = 1
            ENDDO
            IAD = IAD + NIR
            DO JJ=1,NIR
              IK = IAD+JJ
              LLL(IK) = INOD(JJ)
              JLL(IK) = 2
            ENDDO
            IAD = IAD + NIR
            DO JJ=1,NIR
              IK = IAD+JJ
              LLL(IK) = INOD(JJ)
              JLL(IK) = 3
            ENDDO
            IK = IAD + NIR+1
            LLL(IK) = INOD(NIR + 1)
            JLL(IK) = 1
C ---  ic = 2 (vy)
            NC = NC + 1
            IF(NC > LAG_NCF)THEN
              CALL ANCMSG(MSGID=468,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1='INTERFACE TYPE 2',
     .                    C2='INTERFACE TYPE 2',
     .                    C3=TITR)
            ENDIF
            IADLL(NC+1) = IADLL(NC) + NDL
            IF(IADLL(NC+1)-1 > LAG_NKF)THEN
              CALL ANCMSG(MSGID=469,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1='INTERFACE TYPE 2',
     .                    C2='INTERFACE TYPE 2',
     .                    C3=TITR)
            ENDIF
            IAD = IADLL(NC) -1
            DO JJ=1,NIR
              IK = IAD+JJ
              LLL(IK) = INOD(JJ)
              JLL(IK) = 1
            ENDDO
            IAD = IAD + NIR
            DO JJ=1,NIR
              IK = IAD+JJ
              LLL(IK) = INOD(JJ)
              JLL(IK) = 2
            ENDDO
            IAD = IAD + NIR
            DO JJ=1,NIR
              IK = IAD+JJ
              LLL(IK) = INOD(JJ)
              JLL(IK) = 3
            ENDDO
            IK = IAD + NIR+1
            LLL(IK) = INOD(NIR + 1)
            JLL(IK) = 2
C ---  ic = 3 (vz)
            NC = NC + 1
            IF(NC > LAG_NCF)THEN
              CALL ANCMSG(MSGID=468,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1='INTERFACE TYPE 2',
     .                    C2='INTERFACE TYPE 2',
     .                    C3=TITR)
            ENDIF
            IADLL(NC+1) = IADLL(NC) + NDL
            IF(IADLL(NC+1)-1 > LAG_NKF)THEN
              CALL ANCMSG(MSGID=469,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1='INTERFACE TYPE 2',
     .                    C2='INTERFACE TYPE 2',
     .                    C3=TITR)
            ENDIF
            IAD = IADLL(NC) -1
            DO JJ=1,NIR
              IK = IAD+JJ
              LLL(IK) = INOD(JJ)
              JLL(IK) = 1
            ENDDO
            IAD = IAD + NIR
            DO JJ=1,NIR
              IK = IAD+JJ
              LLL(IK) = INOD(JJ)
              JLL(IK) = 2
            ENDDO
            IAD = IAD + NIR
            DO JJ=1,NIR
              IK = IAD+JJ
              LLL(IK) = INOD(JJ)
              JLL(IK) = 3
            ENDDO
            IK = IAD + NIR+1
            LLL(IK) = INOD(NIR+1)
            JLL(IK) = 3
C ---  ic = 4 (wx)
            NC = NC + 1
            IF(NC > LAG_NCF)THEN
              CALL ANCMSG(MSGID=468,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1='INTERFACE TYPE 2',
     .                    C2='INTERFACE TYPE 2',
     .                    C3=TITR)
            ENDIF
            IADLL(NC+1) = IADLL(NC) + NDL
            IF(IADLL(NC+1)-1 > LAG_NKF)THEN
              CALL ANCMSG(MSGID=469,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1='INTERFACE TYPE 2',
     .                    C2='INTERFACE TYPE 2',
     .                    C3=TITR)
            ENDIF
            IAD = IADLL(NC) -1
            DO JJ=1,NIR
              IK = IAD+JJ
              LLL(IK) = INOD(JJ)
              JLL(IK) = 1
            ENDDO
            IAD = IAD + NIR
            DO JJ=1,NIR
              IK = IAD+JJ
              LLL(IK) = INOD(JJ)
              JLL(IK) = 2
            ENDDO
            IAD = IAD + NIR
            DO JJ=1,NIR
              IK = IAD+JJ
              LLL(IK) = INOD(JJ)
              JLL(IK) = 3
            ENDDO
            IK = IAD + NIR + 1
            LLL(IK) = INOD(NIR+1)
            JLL(IK) = 4
C ---  ic = 5 (wy)
            NC = NC + 1
            IF(NC > LAG_NCF)THEN
              CALL ANCMSG(MSGID=468,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1='INTERFACE TYPE 2',
     .                    C2='INTERFACE TYPE 2',
     .                    C3=TITR)
            ENDIF
            IADLL(NC+1) = IADLL(NC) + NDL
            IF(IADLL(NC+1)-1 > LAG_NKF)THEN
              CALL ANCMSG(MSGID=469,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1='INTERFACE TYPE 2',
     .                    C2='INTERFACE TYPE 2',
     .                    C3=TITR)
            ENDIF
            IAD = IADLL(NC) -1
            DO JJ=1,NIR
              IK = IAD+JJ
              LLL(IK) = INOD(JJ)
              JLL(IK) = 1
            ENDDO
            IAD = IAD + NIR
            DO JJ=1,NIR
              IK = IAD+JJ
              LLL(IK) = INOD(JJ)
              JLL(IK) = 2
            ENDDO
            IAD = IAD + NIR
            DO JJ=1,NIR
              IK = IAD+JJ
              LLL(IK) = INOD(JJ)
              JLL(IK) = 3
            ENDDO
            IK = IAD + NIR + 1
            LLL(IK) = INOD(NIR+1)
            JLL(IK) = 5
C ---  ic = 6 (wz)
            NC = NC + 1
            IF(NC > LAG_NCF)THEN
              CALL ANCMSG(MSGID=468,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1='INTERFACE TYPE 2',
     .                    C2='INTERFACE TYPE 2',
     .                    C3=TITR)
            ENDIF
            IADLL(NC+1) = IADLL(NC) + NDL
            IF(IADLL(NC+1)-1 > LAG_NKF)THEN
              CALL ANCMSG(MSGID=469,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1='INTERFACE TYPE 2',
     .                    C2='INTERFACE TYPE 2',
     .                    C3=TITR)
            ENDIF
            IAD = IADLL(NC) -1
            DO JJ=1,NIR
              IK = IAD+JJ
              LLL(IK) = INOD(JJ)
              JLL(IK) = 1
            ENDDO
            IAD = IAD + NIR
            DO JJ=1,NIR
              IK = IAD+JJ
              LLL(IK) = INOD(JJ)
              JLL(IK) = 2
            ENDDO
            IAD = IAD + NIR
            DO JJ=1,NIR
              IK = IAD+JJ
              LLL(IK) = INOD(JJ)
              JLL(IK) = 3
            ENDDO
            IK = IAD + NIR + 1
            LLL(IK) = INOD(NIR+1)
            JLL(IK) = 6
          ENDDO
C---          
        ENDIF
      ENDDO        
C-----------
      RETURN
      END
