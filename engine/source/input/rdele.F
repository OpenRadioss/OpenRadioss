Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  RDELE                         source/input/rdele.F          
Chd|-- called by -----------
Chd|        FREFORM                       source/input/freform.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        READ10P                       source/input/read10p.F        
Chd|        READ5P                        source/input/read5p.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE RDELE(NELOF,KEY,IREC,KEY0)
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
      INTEGER NELOF, IREC
      CHARACTER KEY*5, KEY0*5
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  IV2(10), J, JJ, K, N, K3, NBC
      CHARACTER KEY2*5
C-----------------------------------------------
C
      K=0
      JJ = 0
      DO 600 N=1,NELOF
      READ(IUSC1,REC=IREC+K,FMT='(7X,A,1X,I5,31X,I10)',ERR=999)
     .     KEY2,K3,NBC
      IF(KEY2==KEY)THEN
       IF(K3==1)THEN
        CALL READ10P(IREC+K+1,NBC,KEY0//KEY,IV2,JJ)
       ELSE
        CALL READ5P(IREC+K+1,NBC,KEY0//KEY,IV2,JJ)
       ENDIF
      ENDIF
      K=K+NBC+1
 600  CONTINUE
      IF(JJ/=0) WRITE(IIN,'(10I10)')(IV2(J),J=1,JJ)
      RETURN
 999  CONTINUE
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .            C1=KEY0)
      CALL ARRET(0)
      END
