Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||    freflw        ../engine/source/input/freflw.F
!||--- called by ------------------------------------------------------
!||    freform       ../engine/source/input/freform.F
!||--- calls      -----------------------------------------------------
!||    ancmsg        ../engine/source/output/message/message.F
!||    arret         ../engine/source/system/arret.F
!||    wriusc2       ../engine/source/input/wriusc2.F
!||--- uses       -----------------------------------------------------
!||    message_mod   ../engine/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE FREFLW(IKAD,KEY0,KFLSEC,NSFLSW,NTFLSW)
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
      INTEGER IKAD(0:*), KFLSEC,NSFLSW,NTFLSW
      CHARACTER KEY0(*)*5
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, NBC, K, IKEY, IS, NEL,
     .   NF1, NF2, NF3, NF4, NF5, NF6
      my_real
     .   CRX, CRY, CRZ
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      K=0
      IF(NSFLSW/=0)THEN
       IKEY=KFLSEC
       DO 960 N=1,NSFLSW
        READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(49X,I10)',
     .       ERR=9990)NBC
        K=K+1
        NEL=NBC-1
        NTFLSW = NTFLSW + NEL
        CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
        K=K+1
        READ(IUSC2,*,ERR=9990,END=9990)IS,CRX,CRY,CRZ
        WRITE(IIN,'(2I8,3E16.9)')IS,NEL,CRX,CRY,CRZ
        DO 950 I=1,NEL
         CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
         K=K+1
         READ(IUSC2,*,ERR=9990,END=9990)NF1,NF2,NF3,NF4,NF5,NF6
         WRITE(IIN,'(6I8)')NF1,NF2,NF3,NF4,NF5,NF6
 950    CONTINUE
 960   CONTINUE
      ENDIF
C
      RETURN
C
 9990 CONTINUE
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .            C1=KEY0(IKEY))
      CALL ARRET(0)
      END
