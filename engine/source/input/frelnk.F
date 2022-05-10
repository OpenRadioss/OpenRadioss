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
Chd|  FRELNK                        source/input/frelnk.F         
Chd|-- called by -----------
Chd|        FREFORM                       source/input/freform.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        ERRMSG                        source/input/errmsg.F         
Chd|        READ10                        source/input/read10.F         
Chd|        IXYZ                          source/input/ixyz.F           
Chd|        NVAR                          source/input/nvar.F           
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FRELNK(IKAD,KEY0,KVEL,NRLINK,IFID,VEL_SK_FR)
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
     .        KVEL,NRLINK,IFID,VEL_SK_FR(*)
      CHARACTER KEY0(*)*5
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "task_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "chara_c.inc"
#include      "nchara_c.inc"
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER IXYZ, NVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, NBC, K, KK, NS, K4,IKEY,NVEL
      CHARACTER KEYA*80, CARTE*ncharline, KEY2*5, KEY3*5
C
      K=0
      NVEL = 0 
      LLLINK=0
      IKEY=KVEL
      DO N=1,NRLINK
       NVEL = NVEL + 1
 605  CONTINUE
       READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,1X,A,1X,I5,25X,I10)',
     .        ERR=9990)KEY2,KEY3,K4,NBC
       IF ( VEL_SK_FR(NVEL).GT.99999) K4 = VEL_SK_FR(NVEL)
       K=K+1
       IF(KEY2.EQ.'ALE  ')THEN
        K=K+NBC
        GOTO 605
       ENDIF
       NS=0
       KK=K
       DO I=1,NBC
        READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)',ERR=9990)CARTE
        K=K+1
        NS=NS+NVAR(CARTE)
       ENDDO
C       IFID=IFID+NS
       LLLINK=LLLINK+NS
C version spmd 
       IF(IMACH.EQ.3) THEN
         S_FRLINK = S_FRLINK + 4*NS
       END IF
       IF(KEY2.EQ.'TRA  ')THEN
        WRITE(IIN,'(I8,1X,I3.3,4X,I10)')NS,IXYZ(KEY3),K4
       ELSEIF(KEY2.EQ.'ROT  ')THEN
        WRITE(IIN,'(I8,5X,I3.3,I10)')   NS,IXYZ(KEY3),K4
       ELSE
        CALL ERRMSG(KEY2)
       ENDIF
       CALL READ10(IKAD(IKEY)+KK,NBC,KEY0(IKEY))
      ENDDO
C
      RETURN
C
 9990 CONTINUE
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .            C1=KEY0(IKEY))
      CALL ARRET(0)
      END
