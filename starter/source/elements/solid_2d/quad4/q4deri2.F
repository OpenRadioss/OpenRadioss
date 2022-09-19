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
Chd|  Q4DERI2                       source/elements/solid_2d/quad4/q4deri2.F
Chd|-- called by -----------
Chd|        Q4INIT2                       source/elements/solid_2d/quad4/q4init2.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE Q4DERI2(VOL,KSI,ETA,WI,
     2             Y12,Y34,Y13,Y24,Y14,Y23,
     3             Z12,Z34,Z13,Z24,Z14,Z23,
     4             Y1,Y2,Y3,Y4,YAVG,IHBE, NGL)
      USE MESSAGE_MOD
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
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IHBE, NGL(*)
      my_real
     +   VOL(*),KSI,ETA,WI,
     +   Y12(*),Y34(*),Y13(*),Y24(*),Y14(*),Y23(*),
     +   Z12(*),Z34(*),Z13(*),Z24(*),Z14(*),Z23(*),
     +   Y1(*),Y2(*),Y3(*),Y4(*),YAVG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     +    DET(MVSIZ)
      my_real
     +    QN1,QN2,QN3,QN4,YAVG1
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DO I=LFT,LLT
C       8.*|J|
        DET(I) = (Y34(I)*Z12(I)-Y12(I)*Z34(I))*KSI +
     +           (Y23(I)*Z14(I)-Y14(I)*Z23(I))*ETA +
     +           Y13(I)*Z24(I)-Y24(I)*Z13(I)
C       W*|J|
        VOL(I) = ONE_OVER_8*DET(I)*WI
      ENDDO
C
      IF(N2D==1) THEN
        IF(IHBE==17) THEN
          DO I=LFT,LLT
            VOL(I) = YAVG(I)*VOL(I)
          ENDDO
c        ELSE
c          QN1 = (ONE - ETA)*(ONE - KSI)
c          QN2 = (ONE - ETA)*(KSI + ONE)
c          QN3 = (ETA + ONE)*(KSI + ONE)
c          QN4 = (ETA + ONE)*(ONE - KSI)
c          DO I=LFT,LLT
c            YAVG1 = FOURTH*(QN1*Y1(I)+QN2*Y2(I)+QN3*Y3(I)+QN4*Y4(I))
c            VOL(I) = YAVG1*VOL(I)
c          ENDDO
        ENDIF
      ENDIF
C
      DO I=LFT,LLT
        IF(DET(I)<=ZERO) THEN
C         WRITE(IOUT,1000) NGL(I)
C         CALL ARRET(2)
          CALL ANCMSG(MSGID=228,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=NGL(I))
        ENDIF
      ENDDO
C
      RETURN
      END

