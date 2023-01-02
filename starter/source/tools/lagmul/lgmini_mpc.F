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
Chd|  LGMINI_MPC                    source/tools/lagmul/lgmini_mpc.F
Chd|-- called by -----------
Chd|        LAGM_INI                      source/tools/lagmul/lagm_ini.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LGMINI_MPC(IADLL  ,JLL    ,LLL    ,IBUFNC ,IBUFNN ,
     .                      IBUFDL ,IBUFSK ,NC, MASS, INER, ITAB   ,
     .                      NOM_OPT)
      USE MESSAGE_MOD
C----------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC, IADLL(*), JLL(*), LLL(*), 
     .        IBUFNC(*),IBUFNN(*),IBUFDL(*),IBUFSK(*),
     .        ITAB(*)
      my_real
     .        MASS(*), INER(*)         
      INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "lagmult.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IK, KF, NK, ISK, NN, NDL, NUMC
      my_real COEF
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C======================================================================|
      KF = 0
      DO I=1,NUMMPC
        NC = NC + 1
        IK = IADLL(NC)-1
        NUMC = IBUFNC(I)
        NK = 0
        ID=NOM_OPT(1,I)
        CALL FRETITL2(TITR,
     .       NOM_OPT(LNOPT1-LTITR+1,I),LTITR)
        IF(NC>LAG_NCF)THEN
          CALL ANCMSG(MSGID=468,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1='MULTI-POINT CONSTRAINTS',
     .                C2='MULTI-POINT CONSTRAINTS',
     .                C3=TITR)
        ENDIF
        DO J=1,NUMC
          KF  = KF+1
          NN  = IBUFNN(KF)
          NDL = IBUFDL(KF)
          ISK = IBUFSK(KF)
C---
	  IF(MASS(NN)==0.)THEN
	        CALL ANCMSG(MSGID=535,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  C1='MULTI-POINT CONSTRAINTS',
     .                  I1=ID,
     .                  C2='MULTI-POINT CONSTRAINTS',
     .                  C3=TITR,C4=' ',
     .                  I2=ITAB(NN))
	  ENDIF
          IF(ISK==1)THEN
            NK = NK+1
            IK = IK+1
            LLL(IK) = NN
            JLL(IK) = NDL
          ELSE
            NK = NK+3
            IF (NDL<=3) THEN
              IK = IK+1
              LLL(IK) = NN
              JLL(IK) = 1
              IK = IK + 1
              LLL(IK) = NN
              JLL(IK) = 2
              IK = IK + 1
              LLL(IK) = NN
              JLL(IK) = 3
            ELSE
              IK = IK+1
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
	      IF(NDL>3.AND.INER(NN)==0.)THEN
                CALL ANCMSG(MSGID=536,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      C1='MULTI-POINT CONSTRAINTS',
     .                      I1=ID,
     .                      C2='MULTI-POINT CONSTRAINTS',
     .                      C3=TITR,
     .                      I2=ITAB(NN))
	      ENDIF
          IADLL(NC+1)=IADLL(NC) + NK
          IF(IADLL(NC+1)-1>LAG_NKF)THEN
            CALL ANCMSG(MSGID=469,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1='MULTI-POINT CONSTRAINTS',
     .                  C2='MULTI-POINT CONSTRAINTS',
     .                  C3=TITR)
          ENDIF
        ENDDO
C
      ENDDO
C---
      RETURN
      END
