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
!||    alero2        ../engine/source/ale/ale2d/alero2.F
!||--- called by ------------------------------------------------------
!||    qforc2        ../engine/source/elements/solid_2d/quad/qforc2.F
!||--- calls      -----------------------------------------------------
!||    ancmsg        ../engine/source/output/message/message.F
!||    arret         ../engine/source/system/arret.F
!||--- uses       -----------------------------------------------------
!||    message_mod   ../engine/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE ALERO2(OFF,VOLO,RHON,FLUX,FLU1,W,
     .                  VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4,
     .                  VOLN,DVOL,VD2,
     .                  NC1,NC2,NC3,NC4,NGL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   OFF(*), VOLO(*), RHON(*), FLUX(4,*), FLU1(*), W(3,*),
     .   VY1(*),VY2(*),VY3(*),VY4(*),VZ1(*),VZ2(*),VZ3(*),VZ4(*),VOLN(*), DVOL(*), VD2(*)
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*), NGL(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I
      my_real VY, VZ
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      DO I=LFT,LLT
        RHON(I)=RHON(I)/VOLN(I)
        DVOL(I)=VOLN(I)-VOLO(I)+DT1*HALF*(FLU1(I)+FLUX(1,I)+FLUX(2,I)+FLUX(3,I)+FLUX(4,I))
        VOLO(I)=VOLN(I)
      ENDDO
C
      DO I=LFT,LLT
        VY=VY1(I)+VY2(I)+VY3(I)+VY4(I)-(W(2,NC1(I))+W(2,NC2(I))+W(2,NC3(I))+W(2,NC4(I)))
        VZ=VZ1(I)+VZ2(I)+VZ3(I)+VZ4(I)-(W(3,NC1(I))+W(3,NC2(I))+W(3,NC3(I))+W(3,NC4(I)))
        VD2(I)=(VY**2+VZ**2)/SIXTEEN
      ENDDO
C
      DO I=LFT,LLT
        IF(OFF(I) /= 0.0 .AND. MTN /= 11)THEN
          IF(RHON(I) <= 0.0)THEN
            CALL ANCMSG(MSGID=12,ANMODE=ANINFO,I1=NGL(I),R1=RHON(I))
            CALL ARRET(2)
          ENDIF
        ENDIF
      ENDDO
C-----------------------------------------------
      RETURN
      END
