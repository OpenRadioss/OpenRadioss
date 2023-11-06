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
Chd|  SRCOOR12                      source/elements/solid/solide/srcoor12.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|        SORTHO12                      source/elements/solid/solide/sortho12.F
Chd|        SREPISO12                     source/elements/solid/solide/srepiso12.F
Chd|====================================================================
      SUBROUTINE SRCOOR12(
     1   OFF,     X,       XDP,     NC1,
     2   NC2,     NC3,     NC4,     NC5,
     3   NC6,     NC7,     NC8,     R11,
     4   R12,     R13,     R21,     R22,
     5   R23,     R31,     R32,     R33,
     6   NEL,     JHBE)
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
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: NEL
        INTEGER, INTENT(IN) :: JHBE
C     REAL
        my_real
     .     R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .     R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .     R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .     X(3,*),OFF(*)
        INTEGER
     .          NC1(*),NC2(*),NC3(*),NC4(*),NC5(*),NC6(*),NC7(*),NC8(*)

        DOUBLE PRECISION
     .     XDP(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I

C     REAL
        my_real
     .     RX(MVSIZ) , RY(MVSIZ) , RZ(MVSIZ) ,
     .     SX(MVSIZ) , SY(MVSIZ) , SZ(MVSIZ) ,
     .     TX(MVSIZ) , TY(MVSIZ) , TZ(MVSIZ)
        DOUBLE PRECISION
     .     XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .     XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .     YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .     YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .     ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .     ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ)
C=======================================================================
C
C----------------------------
C     NODAL COORDINATES     |
C----------------------------
        IF(IRESP==1)THEN
          DO I=1,NEL
            IF (OFF(I) <= ONE ) CYCLE
            XD1(I)=XDP(1,NC1(I))
            YD1(I)=XDP(2,NC1(I))
            ZD1(I)=XDP(3,NC1(I))
            XD2(I)=XDP(1,NC2(I))
            YD2(I)=XDP(2,NC2(I))
            ZD2(I)=XDP(3,NC2(I))
            XD3(I)=XDP(1,NC3(I))
            YD3(I)=XDP(2,NC3(I))
            ZD3(I)=XDP(3,NC3(I))
            XD4(I)=XDP(1,NC4(I))
            YD4(I)=XDP(2,NC4(I))
            ZD4(I)=XDP(3,NC4(I))
            XD5(I)=XDP(1,NC5(I))
            YD5(I)=XDP(2,NC5(I))
            ZD5(I)=XDP(3,NC5(I))
            XD6(I)=XDP(1,NC6(I))
            YD6(I)=XDP(2,NC6(I))
            ZD6(I)=XDP(3,NC6(I))
            XD7(I)=XDP(1,NC7(I))
            YD7(I)=XDP(2,NC7(I))
            ZD7(I)=XDP(3,NC7(I))
            XD8(I)=XDP(1,NC8(I))
            YD8(I)=XDP(2,NC8(I))
            ZD8(I)=XDP(3,NC8(I))
          ENDDO
        ELSE
          DO I=1,NEL
            IF (OFF(I) <= ONE ) CYCLE
            XD1(I)=X(1,NC1(I))
            YD1(I)=X(2,NC1(I))
            ZD1(I)=X(3,NC1(I))
            XD2(I)=X(1,NC2(I))
            YD2(I)=X(2,NC2(I))
            ZD2(I)=X(3,NC2(I))
            XD3(I)=X(1,NC3(I))
            YD3(I)=X(2,NC3(I))
            ZD3(I)=X(3,NC3(I))
            XD4(I)=X(1,NC4(I))
            YD4(I)=X(2,NC4(I))
            ZD4(I)=X(3,NC4(I))
            XD5(I)=X(1,NC5(I))
            YD5(I)=X(2,NC5(I))
            ZD5(I)=X(3,NC5(I))
            XD6(I)=X(1,NC6(I))
            YD6(I)=X(2,NC6(I))
            ZD6(I)=X(3,NC6(I))
            XD7(I)=X(1,NC7(I))
            YD7(I)=X(2,NC7(I))
            ZD7(I)=X(3,NC7(I))
            XD8(I)=X(1,NC8(I))
            YD8(I)=X(2,NC8(I))
            ZD8(I)=X(3,NC8(I))
          ENDDO
        ENDIF
C-----------
C     REPERE CONVECTE (ITERATIONS).
C-----------
        CALL SREPISO12(
     1     OFF,     XD1,     XD2,     XD3,
     2     XD4,     XD5,     XD6,     XD7,
     3     XD8,     YD1,     YD2,     YD3,
     4     YD4,     YD5,     YD6,     YD7,
     5     YD8,     ZD1,     ZD2,     ZD3,
     6     ZD4,     ZD5,     ZD6,     ZD7,
     7     ZD8,     RX,      RY,      RZ,
     8     SX,      SY,      SZ,      TX,
     9     TY,      TZ,      NEL)
C---
        IF (JHBE == 14 .OR. JHBE == 24) THEN
          CALL SORTHO12(
     1     OFF,     RX,      RY,      RZ,
     2     SX,      SY,      SZ,      TX,
     3     TY,      TZ,      R12,     R13,
     4     R11,     R22,     R23,     R21,
     5     R32,     R33,     R31,     NEL)
        ELSE
          CALL SORTHO12(
     1     OFF,     RX,      RY,      RZ,
     2     SX,      SY,      SZ,      TX,
     3     TY,      TZ,      R11,     R12,
     4     R13,     R21,     R22,     R23,
     5     R31,     R32,     R33,     NEL)
        ENDIF
C-----------
        RETURN
      END
