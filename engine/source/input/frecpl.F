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
Chd|  FRECPL                        source/input/frecpl.F         
Chd|-- called by -----------
Chd|        FREFORM                       source/input/freform.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        ERRMSG                        source/input/errmsg.F         
Chd|        WRIUSC2                       source/input/wriusc2.F        
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FRECPL(IKAD,KEY0,KCPL)
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
      INTEGER IKAD(0:*),KCPL
      CHARACTER KEY0(*)*5
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "nchar_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J, NBC, K, IKEY, NBCCON
      my_real ULENGTH,UTIME,UMASS,TRAX,TRAY,TRAZ,ROTXX,ROTYY,ROTZZ,ROTXY,ROTXZ,ROTYZ
      CHARACTER KEY2*5,CARTE*ncharline,ONCOD*5
      PARAMETER (NBCCON=1)
      DATA ONCOD/'ONOFF'/
C-----------------------------------------------
      IKEY=KCPL
      K=0
      READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,37X,I10)',ERR=9990) KEY2,NBC
      K=K+1
C-----------------------------------------------
      IF (KEY2=='ON'.OR.KEY2=='ON2'.OR.KEY2=='MPP') THEN
        IF (NBC/=NBCCON) THEN
          CALL ANCMSG(MSGID=73,ANMODE=ANINFO,C1=KEY0(IKEY))
          CALL ARRET(0)
        ELSE
         WRITE(IIN,'(A5)') ONCOD
         CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
         READ(IUSC2,*,ERR=9990,END=9990) ULENGTH,UTIME,UMASS
         WRITE(IIN,'(3G16.9)') ULENGTH,UTIME,UMASS
        END IF
      ELSE
         CALL ERRMSG(KEY2)
         GOTO 9990
      END IF
C
 9999 CONTINUE
      RETURN
C
 9990 CONTINUE
      WRITE(ISTDO,*) ' ** ERROR ** : INPUT ERROR IN OPTION ',KEY0(IKEY)
      WRITE(ISTDO,*) ' ** ERROR ** : INPUT ERROR IN OPTION ',KEY0(IKEY)
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .            C1=KEY0(IKEY))
      CALL ARRET(0)
C
      END
