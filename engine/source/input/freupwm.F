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
Chd|  FREUPWM                       source/input/freupwm.F        
Chd|-- called by -----------
Chd|        FREFORM                       source/input/freform.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        GET_KEYS_ID                   source/input/freoutp.F        
Chd|        WRIUSC2                       source/input/wriusc2.F        
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FREUPWM(IKAD,KEY0,KUPWM)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is reading /UPWM/SUPG card.
C parameter is CUPM=1.0 by default.
C CARD IS OBSOLETE
C In case of /ALE/SUPG/OFF is used then this
C card has no influence and a warning is output
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IKAD(0:*),KUPWM
      CHARACTER KEY0(*)*5
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scrupw_c.inc"
#include      "nchar_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IKEY,ID
      CHARACTER TMPLINE*ncharline
      CHARACTER KEY2*5, KEY3*5,KEY4*5
C
      IKEY=KUPWM
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN              
       READ(IUSC1,REC=IKAD(IKEY),FMT='(A)',ERR=9990)TMPLINE
       CALL GET_KEYS_ID(TMPLINE, KEY2, KEY3,KEY4, ID)       
       IF(KEY3(1:3)/='OFF' .AND. KEY3(1:2)/='ON')THEN
         READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,A4)',ERR=9990)KEY2
         CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY))  
         READ(IUSC2,*,ERR=9990)CUPWM  
       ELSEIF(KEY3(1:3)=='OFF')THEN
          ! /UPWM/SUPG/OFF (obsolete, replaced by /ALE/SUPG/OFF)
           UPWM          = 0
           UPW_UPDATE    = 0
           I_SUPG_ON_OFF = 2
           RETURN
       ENDIF

       IF(CUPWM==0.)CUPWM=1.
       IF(KEY2(1:3)=='STD') THEN
        UPWM=1
       ELSEIF(KEY2(1:2)=='TG') THEN
        UPWM=2
       ELSEIF(KEY2(1:4)=='SUPG')THEN
        I_SUPG_ON_OFF = 1
        UPWM=3
        CALL GET_KEYS_ID(TMPLINE, KEY2, KEY3,KEY4, ID)
        IF(I_SUPG_ON_OFF==2)THEN
         CALL ANCMSG(MSGID=034,ANMODE=ANINFO)
         UPWM=0        
        ENDIF
       ELSE
        GOTO 9990
       ENDIF
      ELSE
       IF(I_SUPG_ON_OFF /= 2)THEN
         UPWM=3
         CUPWM=1.
       ELSE
         UPWM=0
       ENDIF
      ENDIF
C
      RETURN
C
 9990 CONTINUE
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .            C1=KEY0(IKEY))
      CALL ARRET(0)
      END
