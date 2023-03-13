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
Chd|  BLERO2                        source/ale/bimat/blero2.F     
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE BLERO2(ALPH,ALPHC,VOLO,RHON,FLUX,FLU1,OFF,PM,NGL,VOLN,
     .     AIRE, D1, D2, D3, D4, D5, D6, VD2, DVOL, 
     .     VY1, VY2, VY3, VY4,
     .     VZ1, VZ2, VZ3, VZ4, 
     .     MAT)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   ALPH(*), ALPHC(*), VOLO(*), RHON(*), FLUX(4,*), FLU1(*),
     .   OFF(*), PM(NPROPM,*),VOLN(MVSIZ), 
     .   AIRE(*), D1(*), D2(*), D3(*), D4(*), D5(*), D6(*), VD2(*), DVOL(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), 
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*)
      INTEGER MAT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, NGL(*)
      my_real
     .   VY, VZ, SCAL
C-----------------------------------------------

      DO I=LFT,LLT
        VOLN(I)=VOLN(I)*ALPH(I)
        AIRE(I)=AIRE(I)*ALPH(I)
        RHON(I) = RHON(I) / MAX(EM30,VOLN(I))
        DVOL(I) = VOLN(I)-VOLO(I)+DT1*HALF*(FLU1(I)
     .          +FLUX(1,I)+FLUX(2,I)+FLUX(3,I)+FLUX(4,I))
        VOLO(I)=VOLN(I)
      ENDDO

      DO I=LFT,LLT
        IF(ALPH(I)==ZERO)THEN
          RHON(I) =PM( 1,MAT(I))
          DVOL(I) =ZERO
          OFF(I)  =ZERO
          IDEL7NOK = 1
        ENDIF
      ENDDO

      DO I=LFT,LLT
        VY=VY1(I)+VY2(I)+VY3(I)+VY4(I)
        VZ=VZ1(I)+VZ2(I)+VZ3(I)+VZ4(I)
        VD2(I)=(VY**2+VZ**2)/SIXTEEN
      ENDDO

      DO I=LFT,LLT
        IF((MTN==11).OR.(RHON(I)>ZERO))THEN
          IF(ALPH(I)<ONE)THEN
            SCAL=ABS(DVOL(I))/
     .           MAX(EM15,VOLN(I)*DT1*ABS(D1(I)+D2(I)+D3(I)))
            SCAL= MIN(ONE,SCAL)
            D1(I)=D1(I)*SCAL
            D2(I)=D2(I)*SCAL
            D3(I)=D3(I)*SCAL
            D4(I)=D4(I)*SCAL
            D5(I)=D5(I)*SCAL
            D6(I)=D6(I)*SCAL
          ENDIF
        ELSE
          II = NFT+I
          CALL ANCMSG(MSGID=12,ANMODE=ANINFO,
     .                I1=NGL(I),R1=RHON(I))
          CALL ARRET(2)
        ENDIF
      ENDDO !next I

      RETURN
      END
