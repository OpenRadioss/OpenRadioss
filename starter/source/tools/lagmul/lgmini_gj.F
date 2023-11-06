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
Chd|  LGMINI_GJ                     source/tools/lagmul/lgmini_gj.F
Chd|-- called by -----------
Chd|        LAGM_INI                      source/tools/lagmul/lagm_ini.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F
Chd|        INI_DIFF                      source/tools/lagmul/ini_diff.F
Chd|        INI_GEAR                      source/tools/lagmul/ini_gear.F
Chd|        INI_RACK                      source/tools/lagmul/ini_rack.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LGMINI_GJ(IADLL  ,JLL   ,LLL   ,GJBUFI ,NC, MASS,
     .                      INER,ITAB    ,NOM_OPT)
        USE MESSAGE_MOD
C----------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NC, IADLL(*), JLL(*), LLL(*), GJBUFI(LKJNI,*),
     .          ITAB(*)
        my_real
     .          MASS(*), INER(*)
        INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, JTYP, N0, N1, N2, N3, J
        INTEGER ID
        CHARACTER*nchartitle,
     .     TITR
C======================================================================|
        DO I=1,NGJOINT
          JTYP= GJBUFI(2,I)
          N0  = GJBUFI(3,I)
          N1  = GJBUFI(4,I)
          N2  = GJBUFI(5,I)
          N3  = GJBUFI(6,I)
C
          ID=NOM_OPT(1,I)
          CALL FRETITL2(TITR,
     .         NOM_OPT(LNOPT1-LTITR+1,I),LTITR)
          DO J=3,6
            IF(GJBUFI(J,I)/=0)THEN
              IF(MASS(GJBUFI(J,I))==0)THEN
                CALL ANCMSG(MSGID=535,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      C1='GEAR TYPE JOINT',
     .                      I1=ID,
     .                      C2='GEAR TYPE JOINT',
     .                      C3=TITR,C4=' ',
     .                      I2=ITAB(GJBUFI(J,I)))
              ENDIF
            END IF

            IF(GJBUFI(J,I)/=0)THEN
              IF(INER(GJBUFI(J,I))==0)THEN
                CALL ANCMSG(MSGID=536,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      C1='GEAR TYPE JOINT',
     .                      I1=ID,
     .                      C2='GEAR TYPE JOINT',
     .                      C3=TITR,
     .                      I2=ITAB(GJBUFI(J,I)))
              ENDIF
            END IF
          ENDDO

          IF (JTYP==1) THEN
            CALL INI_GEAR(IADLL,JLL  ,LLL  ,N0   ,N1   ,N2,NC,ID,TITR)
          ELSEIF (JTYP==2) THEN
            CALL INI_DIFF(IADLL,JLL  ,LLL  ,N0   ,N1   ,N2,N3,NC,ID,TITR)
          ELSEIF (JTYP==3) THEN
            CALL INI_RACK(IADLL,JLL  ,LLL  ,N0   ,N1   ,N2   ,NC,ID,TITR)
          ENDIF
C
        ENDDO
C---
        RETURN
      END
