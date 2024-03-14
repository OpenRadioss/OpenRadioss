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
Chd|  FREINIV                       source/input/freiniv.F        
Chd|-- called by -----------
Chd|        FREFORM                       source/input/freform.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        ERRMSG                        source/input/errmsg.F         
Chd|        WRIUSC2                       source/input/wriusc2.F        
Chd|        NVAR                          source/input/nvar.F           
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FREINIV(IKAD,KEY0,KINIV,NINIV)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IKAD(0:*),KINIV,NINIV
      CHARACTER KEY0(*)*5
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,M,N,NBC,K,K3,IV1(40),IKEY,JJ,ISK,IFM,IOS,IGN
      my_real V,VTX,VTY,VTZ
      CHARACTER KEY2*5, KEY3*5
      CHARACTER(LEN=NCHARLINE100)::CARTE
C-----------------------------------------------
      K=0
      IOS = 0
      IKEY=KINIV
      DO N=1,NINIV
       READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,1X,A,1X,I5,25X,I10)',
     .        ERR=9990)KEY2,KEY3,K3,NBC
       K=K+1
       IF (KEY2=='TRA  ') THEN
         M=1
         IF(KEY3(1:1)=='X') THEN
           JJ=1
         ELSEIF(KEY3(1:1)=='Y') THEN
           JJ=2
         ELSEIF(KEY3(1:1)=='Z') THEN
           JJ=3
         ELSE
           CALL ERRMSG(KEY2)
         ENDIF
       ELSEIF (KEY2=='ROT  ') THEN
         M=1
         IF(KEY3(1:1)=='X') THEN
           JJ=4
         ELSEIF(KEY3(1:1)=='Y') THEN
           JJ=5
         ELSEIF(KEY3(1:1)=='Z') THEN
           JJ=6
         ELSE
           CALL ERRMSG(KEY2)
         ENDIF
       ELSEIF (KEY2=='AXIS ') THEN
         M=2
         IF(KEY3(1:1)=='X') THEN
           JJ=-1
         ELSEIF(KEY3(1:1)=='Y') THEN
           JJ=-2
         ELSEIF(KEY3(1:1)=='Z') THEN
           JJ=-3
         ELSE
           CALL ERRMSG(KEY2)
         ENDIF
       ELSE
         CALL ERRMSG(KEY2)
       ENDIF
C
       IF (JJ < 0) THEN
c        first line
         CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
         K=K+1
         READ(IUSC2,*,ERR=9990,END=9990) IFM
c        second line
         CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
         K=K+1
         READ(IUSC2,*,ERR=9990,END=9990) VTX, VTY, VTZ, V
       ELSE
         CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
         K=K+1
         READ(IUSC2,*,IOSTAT=IOS) V
       ENDIF
C
       IF(NBC<=M .OR. IOS /= 0 .OR.(K3==2.AND.NBC/=(M+1))) THEN
         CALL ANCMSG(MSGID=73,ANMODE=ANINFO,C1=KEY0(IKEY))
         CALL ARRET(2)
       END IF
C------
       IF (K3==2) THEN
         READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)',ERR=9990)CARTE
         K=K+1
         READ(CARTE,FMT='(I10)') IGN
         WRITE(IIN,'(3I10,1PG20.13)')-IGN,IGN,JJ,V
         IF(JJ < 0) THEN
           WRITE(IIN,'(1P3G20.13,I10)')VTX,VTY,VTZ,IFM
         ENDIF
       ELSE
         DO I=1,NBC-M
           READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)',ERR=9990)CARTE
           CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
           K=K+1
           READ(IUSC2,*,ERR=9990,END=9990)(IV1(J),J=1,NVAR(CARTE))
           DO J=1,NVAR(CARTE),K3+1
             WRITE(IIN,'(3I10,1PG20.13)')IV1(J),IV1(J+K3),JJ,V
             IF(JJ < 0) THEN
               WRITE(IIN,'(1P3G20.13,I10)')VTX,VTY,VTZ,IFM
             ENDIF
           ENDDO
         ENDDO
       END IF
      ENDDO
C
      RETURN
C
 9990 CONTINUE
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .            C1=KEY0(IKEY))
      CALL ARRET(0)
      END
