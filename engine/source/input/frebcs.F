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
Chd|  FREBCS                        source/input/frebcs.F         
Chd|-- called by -----------
Chd|        FREFORM                       source/input/freform.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        ERRMSG                        source/input/errmsg.F         
Chd|        WCIUSC2                       source/input/wciusc2.F        
Chd|        IXYZ                          source/input/ixyz.F           
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FREBCS(IKAD,KEY0,KBCS,KBCSR,NBCS1,NBCS2,BCS_SK_FR)
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
     .        KBCS,KBCSR,NBCS1,NBCS2,BCS_SK_FR(*)
      CHARACTER KEY0(*)*5
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER IXYZ
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, NBC, K, KK, NN, NS, K4,IV1(40),IKEY,NBCS
      CHARACTER KEYA*80, KEY2*5, KEY3*5
C
      K=0
      NBCS = 0
      IKEY = KBCS
      DO NN=1,NBCS1
       NBCS = NBCS + 1
       READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,1X,A,1X,I5,25X,I10)',
     .      ERR=9990)KEY2,KEY3,K4,NBC
       IF ( BCS_SK_FR(NBCS).GT.99999) K4 = BCS_SK_FR(NBCS)
       K=K+1
       KK=K
       NS=0
       DO I=1,NBC
        CALL WCIUSC2(IKAD(IKEY)+K,1,N,KEY0(IKEY))
        READ(IUSC2,*,ERR=9990,END=9990)(IV1(J),J=1,N)
        K=K+1
        IF(KEY2.EQ.'TRA  ')THEN
         WRITE(IIN,'(I10,1X,I3.3,4X,4X,4X,I10)')
     .            (IV1(J),IXYZ(KEY3),K4,J=1,N)
        ELSEIF(KEY2.EQ.'ROT  ')THEN
         WRITE(IIN,'(I10,4X,1X,I3.3,4X,4X,I10)')
     .            (IV1(J),IXYZ(KEY3),K4,J=1,N)
        ELSEIF(KEY2.EQ.'ALE  ')THEN
         WRITE(IIN,'(I10,4X,4X,1X,I3.3,4X,I10)')
     .            (IV1(J),IXYZ(KEY3),K4,J=1,N)
        ELSEIF(KEY2.EQ.'LAG  ')THEN
         WRITE(IIN,'(I10,4X,4X,4X,1X,I3.3,I10)')
     .            (IV1(J),IXYZ(KEY3),K4,J=1,N)
        ELSE
         CALL ERRMSG(KEY2)
        ENDIF
       ENDDO
      ENDDO
C
      K=0
      IKEY=KBCSR
      DO NN=1,NBCS2
       READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,1X,A,1X,I5,25X,I10)',
     .      ERR=9990)KEY2,KEY3,K4,NBC
       K=K+1
       KK=K
       NS=0
       DO I=1,NBC
        CALL WCIUSC2(IKAD(IKEY)+K,1,N,KEY0(IKEY))
        READ(IUSC2,*,ERR=9990,END=9990)(IV1(J),J=1,N)
        K=K+1
        IF(KEY2.EQ.'TRA  ')THEN
         WRITE(IIN,'(I10,1X,I3.3,4X,4X,4X,I10)')
     .            (IV1(J),2*IXYZ(KEY3),K4,J=1,N)
        ELSEIF(KEY2.EQ.'ROT  ')THEN
         WRITE(IIN,'(I10,4X,1X,I3.3,4X,4X,I10)')
     .            (IV1(J),2*IXYZ(KEY3),K4,J=1,N)
        ELSEIF(KEY2.EQ.'ALE  ')THEN
         WRITE(IIN,'(I10,4X,4X,1X,I3.3,4X,I10)')
     .            (IV1(J),2*IXYZ(KEY3),K4,J=1,N)
        ELSEIF(KEY2.EQ.'LAG  ')THEN
         WRITE(IIN,'(I10,4X,4X,4X,1X,I3.3,I10)')
     .            (IV1(J),2*IXYZ(KEY3),K4,J=1,N)
        ELSE
         CALL ERRMSG(KEY2)
        ENDIF
       ENDDO
      ENDDO
C
      RETURN
C
 9990 CONTINUE
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .            C1=KEY0(IKEY))
      CALL ARRET(0)
      END
