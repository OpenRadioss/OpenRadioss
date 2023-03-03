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
Chd|  RKINI3                        source/elements/spring/rkini3.F
Chd|-- called by -----------
Chd|        RINIT3                        source/elements/spring/rinit3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE RKINI3(IFUNCT  ,NPC     ,PLD     ,XK      ,ECROU   , 
     .                  IGEO ,A       ,LSCALE  ,ID,TITR,NOM_OPT)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFUNCT,NPC(*),IGEO(NPROPGI)
      my_real
     .   XK,ECROU,A,LSCALE
      my_real
     .   PLD(*)
      INTEGER ID,ID1
      CHARACTER*nchartitle,
     .   TITR,TITR1
      INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NPOINT, J, J1, HARD,NUMPROP
      my_real
     .   A1, B1, A2, B2, XK_INI
C=======================================================================
      NUMPROP=IGEO(1)
      XK_INI=XK

      IF(IFUNCT==0) RETURN
        NPOINT=(NPC(IFUNCT+1)-NPC(IFUNCT))/2
        DO 50 J=2,NPOINT
        J1=2*(J-2)
        A1=PLD(NPC(IFUNCT)+J1)
        B1=PLD(NPC(IFUNCT)+J1+1)
        A2=PLD(NPC(IFUNCT)+J1+2)
        B2=PLD(NPC(IFUNCT)+J1+3)
        XK= MAX(XK,LSCALE*(B2-B1)/(A2-A1))
  50    CONTINUE
C
        HARD=ECROU
        IF(HARD/=0)THEN
          IF(XK_INI<XK)THEN
            CALL FRETITL2(TITR1,NOM_OPT(LNOPT1-LTITR+1,IFUNCT),LTITR)
            CALL ANCMSG(MSGID=506,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,
     .                  C1=TITR,
     .                  R1=XK_INI,
     .                  R2=XK,
     .                  R3=XK,
     .                  I2=NPC(NFUNCT+IFUNCT+1),
     .                  C2=TITR1,
     .                  PRMOD=MSG_CUMU)
          ENDIF
        ENDIF
C
      RETURN
      END
