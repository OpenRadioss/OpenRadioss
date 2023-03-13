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
Chd|  CHECK_OFF_ALE                 source/elements/solid/solide/check_off_ale.F
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CHECK_OFF_ALE(F11,F21,F31,F12,F22,
     1                           F32,F13,F23,F33,F14,
     2                           F24,F34,F15,F25,F35,
     3                           F16,F26,F36,F17,F27,
     4                           F37,F18,F28,F38,OFF,
     5                           LFT,LLT,NEL)
!       **********************************************
!       For ALE computation, ones needs to set Fij to 
!       0 for inactive elements because the cumulative 
!       terms B are non-zero for ALE computation :
!       Fij = Fij + B
!       ALE --> B /= 0 --> Fij /= 0
!       L   --> B = 0  --> Fij = 0
!       **********************************************
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        ! REAL INOUT ARGUMENT : Fij
        my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: F11,F21,F31,F12,F22
        my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: F32,F13,F23,F33,F14
        my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: F24,F34,F15,F25,F35
        my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: F16,F26,F36,F17,F27
        my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: F37,F18,F28,F38
        ! REAL IN ARGUMENT : OFF
        my_real, DIMENSION(NEL), INTENT(IN) :: OFF
        ! INTEGER IN ARGUMENT : LFT, LLT and NEL
        INTEGER, INTENT(IN) :: LFT,LLT,NEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I
C-----------------------------------------------
        DO I=LFT,LLT
                IF(OFF(I)==ZERO) THEN
                        F11(I) = ZERO
                        F21(I) = ZERO
                        F31(I) = ZERO
                        F12(I) = ZERO
                        F22(I) = ZERO
                        F32(I) = ZERO
                        F13(I) = ZERO
                        F23(I) = ZERO
                        F33(I) = ZERO
                        F14(I) = ZERO
                        F24(I) = ZERO
                        F34(I) = ZERO
                        F15(I) = ZERO
                        F25(I) = ZERO
                        F35(I) = ZERO
                        F16(I) = ZERO
                        F26(I) = ZERO
                        F36(I) = ZERO
                        F17(I) = ZERO
                        F27(I) = ZERO
                        F37(I) = ZERO
                        F18(I) = ZERO
                        F28(I) = ZERO
                        F38(I) = ZERO
                ENDIF
        ENDDO

        RETURN
        END SUBROUTINE CHECK_OFF_ALE
