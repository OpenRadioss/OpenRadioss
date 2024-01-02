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
Chd|  INRCH2                        source/interfaces/inter2d1/inrch2.F
Chd|-- called by -----------
Chd|        I1CHK2                        source/interfaces/inter2d1/i1chk2.F
Chd|        I3STI2                        source/interfaces/inter2d1/i3sti2.F
Chd|        I9STI2                        source/interfaces/int09/i9sti2.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INRCH2(X,IRECT,ICO,ISG,NEL,
     .                  NINT,NOINT, YM1, YM2, ZM1,
     .                  ZM2 ,YE   ,ZE,ID,TITR)
      USE MESSAGE_MOD
C------------------------------------------------
C     RECHERCHE DU NUMERO D'ELEMENT DU SEGMENT
C     CALCUL DES VARIABLES DE /SCR07/
C------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISG, NEL, NINT, NOINT
      INTEGER IRECT(4,*), ICO(7,*)
C     REAL
      my_real
     .   X(3,*), YM1, YM2, ZM1, ZM2,YE(4) ,ZE(4)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IX(4), NE, I1, I2, K, J, JJ, II
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      NEL=0
      IF(NUMELQ==0) RETURN
C
      DO 200 NE=1,NUMELQ
      I1=IRECT(1,ISG)
      I2=IRECT(2,ISG)
      IX(1)=ICO(2,NE)
      IX(2)=ICO(3,NE)
      IX(3)=ICO(4,NE)
      IX(4)=ICO(5,NE)
      DO 180 K=1,4
      IF(IX(K)/=I1)GOTO 180
       DO 120 J=1,4
       IF(IX(J)/=I2)GOTO 120
        NEL=NE
        YM1=X(2,I1)
        ZM1=X(3,I1)
        YM2=X(2,I2)
        ZM2=X(3,I2)
        DO 110 JJ=1,4
        II=IX(JJ)
        YE(JJ)=X(2,II)
        ZE(JJ)=X(3,II)
  110   CONTINUE
        RETURN
  120  CONTINUE
  180 CONTINUE
  200 CONTINUE
C      IWARN=IWARN+1
C      IF(NINT>0) WRITE (IOUT,1100) ISG, NOINT
         IF(NINT>0) THEN
            CALL ANCMSG(MSGID=92,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=ISG)
         ENDIF
C      IF(NINT<0) WRITE (IOUT,1200) ISG, NOINT
         IF(NINT<0) THEN
            CALL ANCMSG(MSGID=93,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=ISG)
         ENDIF
C
      RETURN
C 1100 FORMAT(2X,'** WARNING: CANNOT FIND ELEMENT OF SECONDARY  SEGMENT',I5,
C     +          ' OF INTERFACE',I5)
C 1200 FORMAT(2X,'** WARNING: CANNOT FIND ELEMENT OF MAIN SEGMENT',I5,
C     +          ' OF INTERFACE',I5)
      END
