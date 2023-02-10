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
Chd|  FREABF                        source/input/freabf.F         
Chd|-- called by -----------
Chd|        FREFORM                       source/input/freform.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        WRIUSC2                       source/input/wriusc2.F        
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FREABF(IKAD,KEY0,KABF,ABFILE,NABFILE,CPT)
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
      INTEGER IKAD(0:*),
     .        KABF,ABFILE(*),CPT,NABFILE
      CHARACTER KEY0(*)*5
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "nchar_c.inc"
#include      "chara_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, K, IKEY, N1, KK, J
      CHARACTER KEY2*5, KEY3*5, KEY4*5, CARTE*ncharline
C-----------------------------------------------
      IKEY=KABF

      DTABFWR0(CPT) = ZERO
      K = 0

      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(3X,A,1X,A,1X,A)',
     .      ERR=9990)KEY2,KEY3,KEY4
       K=K+1
       ABFILE(CPT) = ABFILE(CPT) + 1
       NABFILE = NABFILE + 1
       CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
       READ(IUSC2,*,ERR=9990,END=9990)DTABF0(CPT),DTABFWR0(CPT)
       IF (DTABFWR0(CPT) == ZERO) DTABFWR0(CPT) = DTABF0(CPT)
      ELSE
        DTABF0(CPT) = EP30
        TABFIS(CPT) = EP30
      ENDIF
C-----------------------------------------------
      RETURN
C-----------------------------------------------
 9990 CONTINUE
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .            C1=KEY0(IKEY))
      CALL ARRET(0)
      END
