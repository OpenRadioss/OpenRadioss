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
Chd|  Q4RROTA2                      source/elements/solid_2d/quad4/q4rrota2.F
Chd|-- called by -----------
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|        Q4RCOOR2                      source/elements/solid_2d/quad4/q4rcoor2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE Q4RROTA2(
     1   R22,     R23,     R32,     R33,
     2   Y1,      Y2,      Y3,      Y4,
     3   Z1,      Z2,      Z3,      Z4,
     4   NEL)
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
     .   R22(*), R23(*), R32(*), R33(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*)
C-----------------------------------------------
c FUNCTION: 
c ARGUMENTS:  (I: input, O: output, IO: input & output, W: workspace)
c TYPE NAME                FUNCTION
c  I   R22(*)~R33(*)      - [Rij]
c  IO  Y1(*)~Z4(*)        - {X}-->{X'}
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   Y,Z
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C     {X}=[R]{X'} <=> {X'}=T([R]){X}
C     INPUT: [R], {X}
C     OUTPUT: {X'}
      DO I=1,NEL
        Y=R22(I)*Y1(I)+R32(I)*Z1(I)
        Z=R23(I)*Y1(I)+R33(I)*Z1(I)
        Y1(I)=Y
        Z1(I)=Z
        Y=R22(I)*Y2(I)+R32(I)*Z2(I)
        Z=R23(I)*Y2(I)+R33(I)*Z2(I)
        Y2(I)=Y
        Z2(I)=Z
        Y=R22(I)*Y3(I)+R32(I)*Z3(I)
        Z=R23(I)*Y3(I)+R33(I)*Z3(I)
        Y3(I)=Y
        Z3(I)=Z
        Y=R22(I)*Y4(I)+R32(I)*Z4(I)
        Z=R23(I)*Y4(I)+R33(I)*Z4(I)
        Y4(I)=Y
        Z4(I)=Z
      ENDDO
C
      RETURN
      END
            
