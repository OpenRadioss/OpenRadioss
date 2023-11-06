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
Chd|  S8SAV12                       source/elements/solid/solide/s8sav12.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8SAV12(
     1   OFFG,    OFFG0,   SAV,     X,
     2   XDP,     NC1,     NC2,     NC3,
     3   NC4,     NC5,     NC6,     NC7,
     4   NC8,     NEL,     JCVT)
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
        INTEGER, INTENT(IN) :: JCVT
        INTEGER NEL
C     REAL
        INTEGER
     .          NC1(*),NC2(*),NC3(*),NC4(*),NC5(*),NC6(*),NC7(*),NC8(*)
        my_real
     .     OFFG(*),OFFG0(*),X(3,*)
        DOUBLE PRECISION
     .     XDP(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I ,IH
        DOUBLE PRECISION
     .     XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .     XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .     YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .     YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .     ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .     ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ), SAV(NEL,21)
C=======================================================================
C-----Always in global system and fixed then (not like Ismstr=11
C-----for JCVT>0 Fixed SAV is rotated w/ local system not like Ismstr=1
        IH=0
        DO I=1,NEL
          IF(OFFG(I)/=OFFG0(I).AND.ABS(OFFG(I)) > ONE ) IH=1
        END DO
        IF (IH==0) RETURN
C
        IF(IRESP==1)THEN
          DO I=1,NEL
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
C   -----attention, format is different to Ismstr=10,11
        DO I=1,NEL
          IF(OFFG(I)/=OFFG0(I).AND.ABS(OFFG(I)) > ONE )THEN
            SAV(I,1)=XD1(I)-XD8(I)
            SAV(I,2)=YD1(I)-YD8(I)
            SAV(I,3)=ZD1(I)-ZD8(I)
            SAV(I,4)=XD2(I)-XD8(I)
            SAV(I,5)=YD2(I)-YD8(I)
            SAV(I,6)=ZD2(I)-ZD8(I)
            SAV(I,7)=XD3(I)-XD8(I)
            SAV(I,8)=YD3(I)-YD8(I)
            SAV(I,9)=ZD3(I)-ZD8(I)
            SAV(I,10)=XD4(I)-XD8(I)
            SAV(I,11)=YD4(I)-YD8(I)
            SAV(I,12)=ZD4(I)-ZD8(I)
            SAV(I,13)=XD5(I)-XD8(I)
            SAV(I,14)=YD5(I)-YD8(I)
            SAV(I,15)=ZD5(I)-ZD8(I)
            SAV(I,16)=XD6(I)-XD8(I)
            SAV(I,17)=YD6(I)-YD8(I)
            SAV(I,18)=ZD6(I)-ZD8(I)
            SAV(I,19)=XD7(I)-XD8(I)
            SAV(I,20)=YD7(I)-YD8(I)
            SAV(I,21)=ZD7(I)-ZD8(I)
          ENDIF
        ENDDO
C-----------
        RETURN
      END
