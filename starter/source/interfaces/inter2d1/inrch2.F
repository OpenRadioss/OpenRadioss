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
      !||    inrch2                 ../starter/source/interfaces/inter2d1/inrch2.F
      !||--- called by ------------------------------------------------------
      !||    i1chk2                 ../starter/source/interfaces/inter2d1/i1chk2.F
      !||    i3sti2                 ../starter/source/interfaces/inter2d1/i3sti2.F
      !||    i9sti2                 ../starter/source/interfaces/int09/i9sti2.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE INRCH2(X   , IRECT, ICO, ISG, NEL ,
     .                  NINT, YM1, YM2, ZM1 ,
     .                  ZM2 , YE   , ZE , ID , TITR)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C INIT INTERFACE : SEARCH THE CONNECTED ELEMENT TO A GIVEN SEGMENT
C   WHEN FOUND ELEM NODES COORDINATES ARE STORED IN (YE, ZE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISG, NEL, NINT
      INTEGER IRECT(4,*), ICO(7,*)
      my_real X(3,*), YM1, YM2, ZM1, ZM2,YE(4) ,ZE(4)
      INTEGER ID
      CHARACTER(LEN=NCHARTITLE) :: TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IX(4), NE, I1, I2, K, J, JJ, II
      LOGICAL lFOUND
C-----------------------------------------------
C   P r e - C o n d i t i o n
C-----------------------------------------------
      NEL = 0
      IF(NUMELQ == 0) RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO NE=1,NUMELQ
      I1=IRECT(1,ISG)
      I2=IRECT(2,ISG)
      IX(1)=ICO(2,NE)
      IX(2)=ICO(3,NE)
      IX(3)=ICO(4,NE)
      IX(4)=ICO(5,NE)
      DO K=1,4
      IF(IX(K)/=I1)CYCLE
       DO J=1,4
       IF(IX(J)/=I2)CYCLE
        NEL=NE
        YM1=X(2,I1)
        ZM1=X(3,I1)
        YM2=X(2,I2)
        ZM2=X(3,I2)
         DO JJ=1,4
           II=IX(JJ)
           YE(JJ)=X(2,II)
           ZE(JJ)=X(3,II)
         ENDDO !next JJ
         RETURN
        ENDDO !next J
       ENDDO !next K
      END DO !next NE

         IF(NINT>0) THEN
            !** WARNING: CANNOT FIND ELEMENT OF SECONDARY
            CALL ANCMSG(MSGID=92,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_2,I1=ID,C1=TITR,I2=ISG)
         ENDIF

         IF(NINT<0) THEN
            ! ** WARNING: CANNOT FIND ELEMENT OF MAIN SEGMENT
            CALL ANCMSG(MSGID=93,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_2,I1=ID,C1=TITR,I2=ISG)
         ENDIF
      RETURN

      END
