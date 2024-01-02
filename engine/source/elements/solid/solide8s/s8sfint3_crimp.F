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
Chd|  S8SFINT3_CRIMP                source/elements/solid/solide8s/s8sfint3_crimp.F
Chd|-- called by -----------
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8SFINT3_CRIMP(TRM,QF,R,
     .   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .   F15,F25,F35,F16,F26,F36,F17,F27,F37,F18,F28,F38,
     .   NEL   ) 
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*),
     .   F17(*),F27(*),F37(*),F18(*),F28(*),F38(*),
     .   R(3,3,MVSIZ)
      
      DOUBLE PRECISION 
     .   TRM(NEL,24,24),QF(NEL,24),T(24)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      
      DO I=1,NEL
        QF(I,1) = -(R(1,1,I)*F11(I)+R(1,2,I)*F21(I)+R(1,3,I)*F31(I))
        QF(I,2) = -(R(2,1,I)*F11(I)+R(2,2,I)*F21(I)+R(2,3,I)*F31(I))
        QF(I,3) = -(R(3,1,I)*F11(I)+R(3,2,I)*F21(I)+R(3,3,I)*F31(I))
        QF(I,4) = -(R(1,1,I)*F12(I)+R(1,2,I)*F22(I)+R(1,3,I)*F32(I))
        QF(I,5) = -(R(2,1,I)*F12(I)+R(2,2,I)*F22(I)+R(2,3,I)*F32(I))
        QF(I,6) = -(R(3,1,I)*F12(I)+R(3,2,I)*F22(I)+R(3,3,I)*F32(I))
        QF(I,7) = -(R(1,1,I)*F13(I)+R(1,2,I)*F23(I)+R(1,3,I)*F33(I))
        QF(I,8) = -(R(2,1,I)*F13(I)+R(2,2,I)*F23(I)+R(2,3,I)*F33(I))
        QF(I,9) = -(R(3,1,I)*F13(I)+R(3,2,I)*F23(I)+R(3,3,I)*F33(I))
        QF(I,10) = -(R(1,1,I)*F14(I)+R(1,2,I)*F24(I)+R(1,3,I)*F34(I))
        QF(I,11) = -(R(2,1,I)*F14(I)+R(2,2,I)*F24(I)+R(2,3,I)*F34(I))
        QF(I,12) = -(R(3,1,I)*F14(I)+R(3,2,I)*F24(I)+R(3,3,I)*F34(I))
        QF(I,13) = -(R(1,1,I)*F15(I)+R(1,2,I)*F25(I)+R(1,3,I)*F35(I))
        QF(I,14) = -(R(2,1,I)*F15(I)+R(2,2,I)*F25(I)+R(2,3,I)*F35(I))
        QF(I,15) = -(R(3,1,I)*F15(I)+R(3,2,I)*F25(I)+R(3,3,I)*F35(I))
        QF(I,16) = -(R(1,1,I)*F16(I)+R(1,2,I)*F26(I)+R(1,3,I)*F36(I))
        QF(I,17) = -(R(2,1,I)*F16(I)+R(2,2,I)*F26(I)+R(2,3,I)*F36(I))
        QF(I,18) = -(R(3,1,I)*F16(I)+R(3,2,I)*F26(I)+R(3,3,I)*F36(I))
        QF(I,19) = -(R(1,1,I)*F17(I)+R(1,2,I)*F27(I)+R(1,3,I)*F37(I))
        QF(I,20) = -(R(2,1,I)*F17(I)+R(2,2,I)*F27(I)+R(2,3,I)*F37(I))
        QF(I,21) = -(R(3,1,I)*F17(I)+R(3,2,I)*F27(I)+R(3,3,I)*F37(I))
        QF(I,22) = -(R(1,1,I)*F18(I)+R(1,2,I)*F28(I)+R(1,3,I)*F38(I))
        QF(I,23) = -(R(2,1,I)*F18(I)+R(2,2,I)*F28(I)+R(2,3,I)*F38(I))
        QF(I,24) = -(R(3,1,I)*F18(I)+R(3,2,I)*F28(I)+R(3,3,I)*F38(I))
      ENDDO
      
      DO I=1,NEL
        DO J=1,24
          T(J) = TRM(I,1,J)*F11(I)+TRM(I,2,J)*F21(I)+TRM(I,3,J)*F31(I)
     .          +TRM(I,4,J)*F12(I)+TRM(I,5,J)*F22(I)+TRM(I,6,J)*F32(I)
     .          +TRM(I,7,J)*F13(I)+TRM(I,8,J)*F23(I)+TRM(I,9,J)*F33(I)
     .          +TRM(I,10,J)*F14(I)+TRM(I,11,J)*F24(I)+TRM(I,12,J)*F34(I)
     .          +TRM(I,13,J)*F15(I)+TRM(I,14,J)*F25(I)+TRM(I,15,J)*F35(I)
     .          +TRM(I,16,J)*F16(I)+TRM(I,17,J)*F26(I)+TRM(I,18,J)*F36(I)
     .          +TRM(I,19,J)*F17(I)+TRM(I,20,J)*F27(I)+TRM(I,21,J)*F37(I)
     .          +TRM(I,22,J)*F18(I)+TRM(I,23,J)*F28(I)+TRM(I,24,J)*F38(I)
        ENDDO
        F11(I) = T(1)
        F21(I) = T(2)
        F31(I) = T(3)
        F12(I) = T(4)
        F22(I) = T(5)
        F32(I) = T(6)
        F13(I) = T(7)
        F23(I) = T(8)
        F33(I) = T(9)
        F14(I) = T(10)
        F24(I) = T(11)
        F34(I) = T(12)
        F15(I) = T(13)
        F25(I) = T(14)
        F35(I) = T(15)
        F16(I) = T(16)
        F26(I) = T(17)
        F36(I) = T(18)
        F17(I) = T(19)
        F27(I) = T(20)
        F37(I) = T(21)
        F18(I) = T(22)
        F28(I) = T(23)
        F38(I) = T(24)
      ENDDO
      
      RETURN
      END
      
