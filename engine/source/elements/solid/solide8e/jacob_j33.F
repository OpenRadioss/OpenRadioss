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
Chd|  JACOB_J33                     source/elements/solid/solide8e/jacob_j33.F
Chd|-- called by -----------
Chd|        JACOB_F0                      source/elements/solid/solide8e/jacob_f0.F
Chd|        S10VOLNODT3                   source/elements/solid/solide4_sfem/s10volnodt3.F
Chd|        S10_ICP                       source/elements/solid/solide4_sfem/s10_icp.F
Chd|        S4VOLN_M                      source/elements/solid/solide4_sfem/s4voln_m.F
Chd|        S8EDEFOT3                     source/elements/solid/solide8e/s8edefot3.F
Chd|        S8ZDEFOT3                     source/elements/solid/solide8z/s8zdefot3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE JACOB_J33(
     1   DET,     AJ1,     AJ2,     AJ3,
     2   AJ4,     AJ5,     AJ6,     AJ7,
     3   AJ8,     AJ9,     NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
C     REAL
      my_real
     .   AJ1(*),AJ2(*),AJ3(*),
     .   AJ4(*),AJ5(*),AJ6(*),
     .   AJ7(*),AJ8(*),AJ9(*),
     .   DET(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
C                                                                     12
      my_real
     .   JAC_59_68, JAC_67_49, JAC_48_57,
     .   AJ11, AJ22, AJ33
C-----------------------------------------------
C     JACOBIAN of [AJ]+[1]
C
      DO I=1,NEL
       AJ11=AJ1(I)+ONE
       AJ22=AJ5(I)+ONE
       AJ33=AJ9(I)+ONE
       JAC_59_68=AJ22*AJ33-AJ6(I)*AJ8(I)
       JAC_67_49=AJ6(I)*AJ7(I)-AJ4(I)*AJ33
       JAC_48_57=AJ4(I)*AJ8(I)-AJ22*AJ7(I)
C
       DET(I)=AJ11*JAC_59_68+AJ2(I)*JAC_67_49+AJ3(I)*JAC_48_57
      ENDDO
C
      RETURN
      END
