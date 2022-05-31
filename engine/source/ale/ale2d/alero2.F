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
Chd|  ALERO2                        source/ale/ale2d/alero2.F     
Chd|-- called by -----------
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ALERO2(OFF,VOLO,RHON,FLUX,FLU1,W,
     .                  VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4,
     .                  VOLN,DVOL,VD2,
     .                  NC1,NC2,NC3,NC4,NGL)
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      my_real
     .   OFF(*), VOLO(*), RHON(*), FLUX(4,*), FLU1(*), W(3,*),
     .   VY1(*),VY2(*),VY3(*),VY4(*),VZ1(*),VZ2(*),VZ3(*),VZ4(*),
     .   VOLN(*), DVOL(*), VD2(*)
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*), NGL(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I
C     REAL ou REAL*8
      my_real
     .    VY, VZ
C-----------------------------------------------
C
      DO 20 I=LFT,LLT
      RHON(I)    =RHON(I)/VOLN(I)
      DVOL(I) = VOLN(I)-VOLO(I)+DT1*HALF*(FLU1(I)
     .  +FLUX(1,I)+FLUX(2,I)+FLUX(3,I)+FLUX(4,I))
      VOLO(I)=VOLN(I)
   20 CONTINUE
C
      DO 30 I=LFT,LLT
      VY=VY1(I)+VY2(I)+VY3(I)+VY4(I)
     . -(W(2,NC1(I))+W(2,NC2(I))+W(2,NC3(I))+W(2,NC4(I)))
      VZ=VZ1(I)+VZ2(I)+VZ3(I)+VZ4(I)
     . -(W(3,NC1(I))+W(3,NC2(I))+W(3,NC3(I))+W(3,NC4(I)))
      VD2(I)=(VY**2+VZ**2)/SIXTEEN
   30 CONTINUE
C
      DO 40 I=LFT,LLT
      IF(OFF(I).EQ.0.0.OR.MTN.EQ.11)GO TO 40
      IF(RHON(I).GT.0.0)GO TO 40
      CALL ANCMSG(MSGID=12,ANMODE=ANINFO,
     .            I1=NGL(I),R1=RHON(I))
      CALL ARRET(2)
   40 CONTINUE
C
      RETURN
      END
