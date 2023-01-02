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
Chd|  FREDAMP                       source/input/fredamp.F        
Chd|-- called by -----------
Chd|        FREFORM                       source/input/freform.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        WRIUSC2                       source/input/wriusc2.F        
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FREDAMP(IKAD,KEY0,KDAMP,NDAMP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IKAD(0:*),KDAMP,NDAMP
      CHARACTER KEY0(*)*5
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, NN, ID, IKEY, IDG, IGR, NBC, KORTH
      my_real DAMPA, DAMPB
      character carte*100
C=======================================================================
      K=0
      IKEY = KDAMP
      IGR = 0
      IF (NDAMP == 1) THEN
        READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,I5,37X,I10)',ERR=9990)
     .                                                       ID,NBC
        IF (ID/=1.AND.NBC==1) THEN
          KORTH=0
          K=K+1
          CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
          READ(IUSC2,*,ERR=9990,END=9990) DAMPA,DAMPB
          WRITE(IIN,'(I10,1P2G20.13,2I10)') ID,DAMPA,DAMPB,IGR,KORTH
        ELSEIF(ID==1) THEN
          IF(NBC==2) THEN
            K=K+1
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
            READ(IUSC2,*,ERR=9990,END=9990) IGR
          ENDIF
          IF(NBC/=6)THEN
            KORTH=0
            K=K+1
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
            READ(IUSC2,*,ERR=9990,END=9990) DAMPA,DAMPB
            WRITE(IIN,'(I10,1P2G20.13,2I10)') ID,DAMPA,DAMPB,IGR,KORTH
          ELSE
            KORTH=1
C           dampax, dampbx
            K=K+1
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
            READ(IUSC2,*,ERR=9990,END=9990) DAMPA,DAMPB
            WRITE(IIN,'(I10,1P2G20.13,2I10)') ID,DAMPA,DAMPB,IGR,KORTH
C           dampay, dampby
            K=K+1
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
            READ(IUSC2,*,ERR=9990,END=9990) DAMPA,DAMPB
            WRITE(IIN,'(1P2G20.13)') DAMPA,DAMPB
C           dampaz, dampbz
            K=K+1
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
            READ(IUSC2,*,ERR=9990,END=9990) DAMPA,DAMPB
            WRITE(IIN,'(1P2G20.13)') DAMPA,DAMPB
C           damparx, dampbrx
            K=K+1
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
            READ(IUSC2,*,ERR=9990,END=9990) DAMPA,DAMPB
            WRITE(IIN,'(1P2G20.13)') DAMPA,DAMPB
C           dampary, dampbry
            K=K+1
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
            READ(IUSC2,*,ERR=9990,END=9990) DAMPA,DAMPB
            WRITE(IIN,'(1P2G20.13)') DAMPA,DAMPB
C           damparz, damprbz
            K=K+1
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
            READ(IUSC2,*,ERR=9990,END=9990) DAMPA,DAMPB
            WRITE(IIN,'(1P2G20.13)') DAMPA,DAMPB
          END IF
        ELSE
          GOTO 9990
        ENDIF
      ELSE
        DO NN=1,NDAMP
          READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,I5,37X,I10)',ERR=9990)
     .                                                         ID,NBC
          IF(NBC/=6)THEN
            KORTH=0
            K=K+1
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
            READ(IUSC2,*,ERR=9990,END=9990) DAMPA,DAMPB
            WRITE(IIN,'(I10,1P2G20.13,10X,I10)') ID,DAMPA,DAMPB,KORTH
          ELSE
            KORTH=1
C           dampax, dampbx
            K=K+1
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
            READ(IUSC2,*,ERR=9990,END=9990) DAMPA,DAMPB
            WRITE(IIN,'(I10,1P2G20.13,10X,I10)') ID,DAMPA,DAMPB,KORTH
C           dampay, dampby
            K=K+1
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
            READ(IUSC2,*,ERR=9990,END=9990) DAMPA,DAMPB
            WRITE(IIN,'(1P2G20.13)') DAMPA,DAMPB
C           dampaz, dampbz
            K=K+1
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
            READ(IUSC2,*,ERR=9990,END=9990) DAMPA,DAMPB
            WRITE(IIN,'(1P2G20.13)') DAMPA,DAMPB
C           damparx, dampbrx
            K=K+1
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
            READ(IUSC2,*,ERR=9990,END=9990) DAMPA,DAMPB
            WRITE(IIN,'(1P2G20.13)') DAMPA,DAMPB
C           dampary, dampbry
            K=K+1
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
            READ(IUSC2,*,ERR=9990,END=9990) DAMPA,DAMPB
            WRITE(IIN,'(1P2G20.13)') DAMPA,DAMPB
C           damparz, damprbz
            K=K+1
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
            READ(IUSC2,*,ERR=9990,END=9990) DAMPA,DAMPB
            WRITE(IIN,'(1P2G20.13)') DAMPA,DAMPB
          END IF
          K=K+1
        ENDDO
      ENDIF
C---
      RETURN
 9990 CONTINUE
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .            C1=KEY0(IKEY))
      CALL ARRET(0)
      END
