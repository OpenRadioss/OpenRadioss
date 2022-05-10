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
Chd|  FREFVBAG                      source/input/frefvbag.F       
Chd|-- called by -----------
Chd|        FREFORM                       source/input/freform.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        WRIUSC2                       source/input/wriusc2.F        
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FREFVBAG(IKAD,KEY0,KFVBAG)
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
      INTEGER IKAD(0:*),KFVBAG
      CHARACTER KEY0(*)*5
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IKEY, II, I, FVID, NSTEP, N1, N2, JJ, NBC, IV2(10), J, N3
      my_real
     .        VAL1, VAL2, VAL3
      CHARACTER KEY2*5
C
      IKEY=KFVBAG
      II=0
      IF (IKAD(IKEY).EQ.IKAD(IKEY+1)) RETURN
  100 CALL WRIUSC2(IKAD(IKEY)+II,1,KEY0(IKEY))
      READ(IUSC2,'(7X,A5,37X,I5)') KEY2, NBC
      IF (KEY2.EQ.'REMES') THEN
C /FVMBAG/REMESH
         WRITE(IIN,'(I10)') 1
C
         II=II+1
         CALL WRIUSC2(IKAD(IKEY)+II,1,KEY0(IKEY))
         READ(IUSC2,*,ERR=9990,END=9990) FVID, NSTEP
         WRITE(IIN,'(2I10)') FVID, NSTEP
         II=II+1
         CALL WRIUSC2(IKAD(IKEY)+II,1,KEY0(IKEY))
         READ(IUSC2,*,ERR=9990,END=9990) VAL1, VAL2, VAL3
         WRITE(IIN,'(1P3G20.13)') VAL1, VAL2, VAL3
         II=II+1
         CALL WRIUSC2(IKAD(IKEY)+II,1,KEY0(IKEY))
         READ(IUSC2,*,ERR=9990,END=9990) VAL1, VAL2, VAL3
         WRITE(IIN,'(1P3G20.13)') VAL1, VAL2, VAL3
         II=II+1
         CALL WRIUSC2(IKAD(IKEY)+II,1,KEY0(IKEY))
         READ(IUSC2,*,ERR=9990,END=9990) VAL1, VAL2, VAL3
         WRITE(IIN,'(1P3G20.13)') VAL1, VAL2, VAL3
         II=II+1
         CALL WRIUSC2(IKAD(IKEY)+II,1,KEY0(IKEY))
         READ(IUSC2,*,ERR=9990,END=9990) VAL1, VAL2, VAL3
         WRITE(IIN,'(1P3G20.13)') VAL1, VAL2, VAL3
         II=II+1
         CALL WRIUSC2(IKAD(IKEY)+II,1,KEY0(IKEY))
         READ(IUSC2,*,ERR=9990,END=9990) N1, N2, N3
         WRITE(IIN,'(3I10)') N1, N2, N3
C
      ELSEIF (KEY2.EQ.'MODIF') THEN
C /FVMBAG/MODIF
         II=II+1
         CALL WRIUSC2(IKAD(IKEY)+II,1,KEY0(IKEY))
         READ(IUSC2,*,ERR=9990,END=9990) FVID
         WRITE(IIN,'(I10)') FVID
         II=II+1
         CALL WRIUSC2(IKAD(IKEY)+II,1,KEY0(IKEY))
         READ(IUSC2,*,ERR=9990,END=9990)  N1, VAL1, VAL2, N2
         WRITE(IIN,'(I10,1P2G20.13,I10)') N1, VAL1, VAL2, N2
      ENDIF
      II=II+1
      IF (IKAD(IKEY)+II.NE.IKAD(IKEY+1)) GO TO 100
C      
      RETURN
C
 9990 CONTINUE
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .            C1=KEY0(IKEY))
      CALL ARRET(0)
      END
