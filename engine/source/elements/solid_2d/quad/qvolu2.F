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
Chd|  QVOLU2                        source/elements/solid_2d/quad/qvolu2.F
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|        MULTI_COMPUTEVOLUME           source/multifluid/multi_computevolume.F
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|        Q4KE2                         source/elements/solid_2d/quad4/q4ke2.F
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE QVOLU2(
     1   OFF,     AIRE,    VOLU,    NGL,
     2   Y1,      Y2,      Y3,      Y4,
     3   Z1,      Z2,      Z3,      Z4,
     4   Y234,    Y124,    NEL,     JMULT,
     5   JCVT)
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
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: JMULT
      INTEGER, INTENT(IN) :: JCVT
      INTEGER, INTENT(IN) :: NGL(NEL)
      my_real, INTENT(IN) ::  Y1(NEL),Y2(NEL),Y3(NEL),Y4(NEL),Z1(NEL),Z2(NEL),Z3(NEL),Z4(NEL),Y234(NEL),Y124(NEL)
      my_real, INTENT(INOUT) :: OFF(NEL),AIRE(NEL),VOLU(NEL)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real A1(MVSIZ), A2(MVSIZ)
C-----------------------------------------------
C  S o u r c e   L i n e s
C-----------------------------------------------
      DO I=1,NEL
        A1(I) =Y2(I)*(Z3(I)-Z4(I))+Y3(I)*(Z4(I)-Z2(I))+Y4(I)*(Z2(I)-Z3(I))
        A2(I) =Y2(I)*(Z4(I)-Z1(I))+Y4(I)*(Z1(I)-Z2(I))+Y1(I)*(Z2(I)-Z4(I))
        AIRE(I)=(A1(I)+A2(I))*HALF
      ENDDO
C-----------------------------------------------
      IF(N2D == 1) THEN
        IF(JCVT == 0)THEN
          DO I=1,NEL
            VOLU(I)=((Y2(I)+Y3(I)+Y4(I))*A1(I)+(Y1(I)+Y2(I)+Y4(I))*A2(I))*ONE_OVER_6
          ENDDO
        ELSE
          DO I=1,NEL
           VOLU(I)=(Y234(I)*A1(I)+Y124(I)*A2(I))*ONE_OVER_6
          ENDDO
        END IF
      ELSE
        DO I=1,NEL
          VOLU(I)=AIRE(I)
        ENDDO
      ENDIF
C-----------------------------------------------
      IF(JMULT == 0)THEN
        DO I=1,NEL
          IF(OFF(I) == ZERO)THEN
            VOLU(I)=ONE
            AIRE(I)=ONE
          ELSE
            IF(VOLU(I) <= VOLMIN)THEN
              VOLU(I)=ONE
              OFF(I)=ZERO
              CALL ANCMSG(MSGID=142,ANMODE=ANINFO,I1=NGL(I))
              IDEL7NOK = 1
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------
      DO I=1,NEL
        IF(OFF(I) >= ONE .AND. VOLU(I) <= ZERO) THEN
          CALL ANCMSG(MSGID=142,ANMODE=ANINFO,I1=NGL(I))
          CALL ARRET(2)
        ENDIF
      ENDDO
C-----------------------------------------------
      RETURN
      END
