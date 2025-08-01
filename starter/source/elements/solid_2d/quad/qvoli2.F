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
C
Cls -> Cgw|         ARRET                           /sortie1/arret.F
!||====================================================================
!||    qvoli2             ../starter/source/elements/solid_2d/quad/qvoli2.F
!||--- called by ------------------------------------------------------
!||    binit2             ../starter/source/ale/bimat/binit2.F
!||    multifluid_init2   ../starter/source/multifluid/multifluid_init2.F
!||    qinit2             ../starter/source/elements/solid_2d/quad/qinit2.F
!||--- calls      -----------------------------------------------------
!||    ancmsg             ../starter/source/output/message/message.F
!||--- uses       -----------------------------------------------------
!||    message_mod        ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE QVOLI2(VOLU,IXQ, 
     .     NGL, AIRE, 
     .     Y1, Y2, Y3, Y4, 
     .     Z1, Z2, Z3, Z4)
      USE MESSAGE_MOD
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXQ(NIXQ,*), NGL(*)
      my_real
     .   VOLU(*), AIRE(*), 
     .   Y1(*), Y2(*), Y3(*), Y4(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*)
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
        AIRE(I)=(A1(I)+A2(I))* HALF
        IF (AIRE(I)==ZERO) THEN
           CALL ANCMSG(MSGID=227,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=IXQ(NIXQ,I))
        ELSEIF(AIRE(I)<ZERO.AND.N2D==1) THEN
                     CALL ANCMSG(MSGID=1228,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=NGL(I),
     .                 PRMOD=MSG_CUMU)
         AIRE(I) = -AIRE(I)
        ENDIF
      END DO

      IF(N2D==1) THEN
        DO I=LFT,LLT
          VOLU(I)=((Y2(I)+Y3(I)+Y4(I))*A1(I)+
     .             (Y1(I)+Y2(I)+Y4(I))*A2(I))* ONE_OVER_6
        END DO
      ELSE
        DO I=LFT,LLT
          VOLU(I)=AIRE(I)
          END DO
      ENDIF

      DO I=LFT,LLT
        IF(VOLU(I)<=0.0)THEN
          CALL ANCMSG(MSGID=228,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=NGL(I))
            EXIT
        END IF
      END DO

      RETURN

      END
