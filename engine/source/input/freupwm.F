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
      !||====================================================================
      !||    freupwm                ../engine/source/input/freupwm.F
      !||--- called by ------------------------------------------------------
      !||    freform                ../engine/source/input/freform.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../engine/source/output/message/message.F
      !||    arret                  ../engine/source/system/arret.F
      !||    get_keys_id            ../engine/source/input/freoutp.F
      !||    wriusc2                ../engine/source/input/wriusc2.F
      !||--- uses       -----------------------------------------------------
      !||    ale_mod                ../common_source/modules/ale/ale_mod.F
      !||    message_mod            ../engine/share/message_module/message_mod.F
      !||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
      !||====================================================================
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
      USE ALE_MOD
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100
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
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IKEY,ID
      CHARACTER(LEN=NCHARLINE100):: TMPLINE
      CHARACTER KEY2*5, KEY3*5,KEY4*5
C
      IKEY=KUPWM
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN              
       READ(IUSC1,REC=IKAD(IKEY),FMT='(A)',ERR=9990)TMPLINE
       CALL GET_KEYS_ID(TMPLINE, KEY2, KEY3,KEY4, ID)       
       IF(KEY3(1:3)/='OFF' .AND. KEY3(1:2)/='ON')THEN
         READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,A4)',ERR=9990)KEY2
         CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY))  
         READ(IUSC2,*,ERR=9990)ALE%UPWIND%CUPWM  
       ELSEIF(KEY3(1:3)=='OFF')THEN
          ! /UPWM/SUPG/OFF (obsolete, replaced by /ALE/SUPG/OFF)
           ALE%UPWIND%UPWM          = 0
           ALE%UPWIND%UPW_UPDATE    = 0
           ALE%UPWIND%I_SUPG_ON_OFF = 2
           RETURN
       ENDIF

       IF(ALE%UPWIND%CUPWM==0.)ALE%UPWIND%CUPWM=1.
       IF(KEY2(1:3)=='STD') THEN
         ALE%UPWIND%UPWM=1
       ELSEIF(KEY2(1:2)=='TG') THEN
         ALE%UPWIND%UPWM=2
       ELSEIF(KEY2(1:4)=='SUPG')THEN
         ALE%UPWIND%I_SUPG_ON_OFF = 1
         ALE%UPWIND%UPWM=3
         CALL GET_KEYS_ID(TMPLINE, KEY2, KEY3,KEY4, ID)
        IF(ALE%UPWIND%I_SUPG_ON_OFF==2)THEN
         CALL ANCMSG(MSGID=034,ANMODE=ANINFO)
         ALE%UPWIND%UPWM=0        
        ENDIF
       ELSE
        GOTO 9990
       ENDIF
      ELSE
       IF(ALE%UPWIND%I_SUPG_ON_OFF /= 2)THEN
         ALE%UPWIND%UPWM=3
         ALE%UPWIND%CUPWM=1.
       ELSE
         ALE%UPWIND%UPWM=0
       ENDIF
      ENDIF
C
      RETURN
C
 9990 CONTINUE
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,C1=KEY0(IKEY))
      CALL ARRET(0)
      END
