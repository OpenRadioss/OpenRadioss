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
Chd|  FREDLI                        source/input/fredli.F         
Chd|-- called by -----------
Chd|        FREFORM                       source/input/freform.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        READ10P                       source/input/read10p.F        
Chd|        WRIUSC2                       source/input/wriusc2.F        
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FREDLI(IKAD,KEY0,KDEL,NDEL)
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
      INTEGER IKAD(0:*),KDEL,NDEL
      CHARACTER KEY0(*)*5
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J, N, NBC, K, IKEY, JJ, K3, IV2(10),INT
      CHARACTER KEY2*5,KEY3*5
C
      K=0
      JJ = 0
      IKEY=KDEL
      DO N=1,NDEL
        READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,1X,A,31X,I10)',
     .     ERR=9990)KEY2,KEY3,NBC
        IF(KEY2=='INTER'.AND.
     .   KEY3(1:3)/='NOD'.AND.KEY3(1:3)/='SEG')
     .   CALL READ10P(IKAD(IKEY)+K+1,NBC,KEY0(IKEY),IV2,JJ)
        K=K+NBC+1
      ENDDO
      IF(JJ/=0)WRITE(IIN,'(10I10)')(IV2(J),J=1,JJ)
C
      K=0
      JJ = 0
      DO N=1,NDEL
        READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,1X,A,31X,I10)',
     .     ERR=9990)KEY2,KEY3,NBC
        IF(KEY2=='INTER'.AND.KEY3(1:3)=='NOD')THEN
          CALL WRIUSC2(IKAD(IKEY)+K+1,1,KEY0(IKEY))
          READ(IUSC2,*,ERR=9990,END=9990)INT
          WRITE(IIN,'(2I8)')INT,NBC-1
          CALL READ10P(IKAD(IKEY)+K+2,NBC-1,KEY0(IKEY),IV2,JJ)
        ENDIF
        K=K+NBC+1
      ENDDO
      IF(JJ/=0)WRITE(IIN,'(10I10)')(IV2(J),J=1,JJ)
C
      K=0
      JJ = 0
      DO N=1,NDEL
        READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,1X,A,31X,I10)',
     .     ERR=9990)KEY2,KEY3,NBC
        IF(KEY2=='INTER'.AND.KEY3(1:3)=='SEG')THEN
          CALL WRIUSC2(IKAD(IKEY)+K+1,1,KEY0(IKEY))
          READ(IUSC2,*,ERR=9990,END=9990)INT
          WRITE(IIN,'(2I8)')INT,NBC-1
          CALL READ10P(IKAD(IKEY)+K+2,NBC-1,KEY0(IKEY),IV2,JJ)
        ENDIF
        K=K+NBC+1
      ENDDO
      IF(JJ/=0)WRITE(IIN,'(10I10)')(IV2(J),J=1,JJ)
C
      RETURN
C
 9990 CONTINUE
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .            C1=KEY0(IKEY))
      CALL ARRET(0)
      END
