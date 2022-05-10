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
Chd|  Q4VOLI2                       source/elements/solid_2d/quad4/q4voli2.F
Chd|-- called by -----------
Chd|        Q4INIT2                       source/elements/solid_2d/quad4/q4init2.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE Q4VOLI2(VOLU,IXQ, 
     .     NGL, AIRE, 
     .     Y1, Y2, Y3, Y4, 
     .     Z1, Z2, Z3, Z4,Y234,Y124)
      USE MESSAGE_MOD
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXQ(NIXQ,*), NGL(*)
      my_real
     .   VOLU(*), AIRE(*), 
     .   Y1(*), Y2(*), Y3(*), Y4(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*),Y234(*),Y124(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   A1(MVSIZ), A2(MVSIZ)
C----------------------------------------------------------------------
      DO I=LFT,LLT
        A1(I) =Y2(I)*(Z3(I)-Z4(I))+
     .         Y3(I)*(Z4(I)-Z2(I))+Y4(I)*(Z2(I)-Z3(I))
        A2(I) =Y2(I)*(Z4(I)-Z1(I))+
     .         Y4(I)*(Z1(I)-Z2(I))+Y1(I)*(Z2(I)-Z4(I))
        AIRE(I)=(A1(I)+A2(I))* UNDEMI
        IF (AIRE(I).EQ.ZERO) THEN
           CALL ANCMSG(MSGID=227,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=IXQ(NIXQ,I))
        ELSEIF(AIRE(I).LT.ZERO.AND.N2D==1) THEN
                     CALL ANCMSG(MSGID=1228,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=NGL(I),
     .                 PRMOD=MSG_CUMU)
         AIRE(I) = -AIRE(I)
        ENDIF
      END DO

      IF(N2D.EQ.1) THEN
       IF(JCVT.EQ.0)THEN
        DO I=LFT,LLT
         VOLU(I)=((Y2(I)+Y3(I)+Y4(I))*A1(I)
     .                 +(Y1(I)+Y2(I)+Y4(I))*A2(I))*UNSIX
        END DO
       ELSE
        DO I=LFT,LLT
         VOLU(I)=(Y234(I)*A1(I)+Y124(I)*A2(I))*UNSIX
        END DO
       END IF
      ELSE
        DO I=LFT,LLT
          VOLU(I)=AIRE(I)
	END DO
      ENDIF

      DO I=LFT,LLT
        IF(VOLU(I).LE.0.0)THEN
          CALL ANCMSG(MSGID=228,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=NGL(I))
	  EXIT	
        END IF
      END DO

      RETURN

      END
