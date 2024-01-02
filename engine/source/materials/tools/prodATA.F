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
C This subroutines computes C(NEL,3,3) which is the product [C] = [A]t[A]
Chd|====================================================================
Chd|  PRODATA                       source/materials/tools/prodATA.F
Chd|-- called by -----------
Chd|        SIGEPS190                     source/materials/mat/mat190/sigeps190.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRODATA(A, C, E, NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      INTEGER,  INTENT(IN) ::  NEL
      my_real,  DIMENSION(NEL,3, 3),INTENT(IN) ::  A
      my_real,  DIMENSION(NEL,3, 3),INTENT(OUT) :: C 
      my_real,  DIMENSION(NEL,6)   ,INTENT(OUT) :: E 
C
      INTEGER  I
C-----------------------------------------------
      DO I =1, NEL                                      
        C(I,1,1) = A(I,1,1)**2 + A(I,2,1)**2 + A(I,3,1)**2
        C(I,2,2) = A(I,1,2)**2 + A(I,2,2)**2 + A(I,3,2)**2
        C(I,3,3) = A(I,1,3)**2 + A(I,2,3)**2 + A(I,3,3)**2

        C(I,1,2) = A(I,1,1)*A(I,1,2) + A(I,2,1)*A(I,2,2) + A(I,3,1)*A(I,3,2)
        C(I,1,3) = A(I,1,1)*A(I,1,3) + A(I,2,1)*A(I,2,3) + A(I,3,1)*A(I,3,3)
        C(I,2,3) = A(I,1,2)*A(I,1,3) + A(I,2,2)*A(I,2,3) + A(I,3,2)*A(I,3,3)

        C(I,2,1) = C(I,1,2) 
        C(I,3,1) = C(I,1,3) 
        C(I,3,2) = C(I,2,3) 


        !GREEN LAGRANGE
        E(I,1) = (C(I,1,1) - ONE) /TWO
        E(I,2) = (C(I,2,2) - ONE) /TWO
        E(I,3) = (C(I,3,3) - ONE) /TWO
        E(I,4) =  C(I,1,2) /TWO
        E(I,5) =  C(I,2,3) /TWO
        E(I,6) =  C(I,1,3) /TWO
C
      ENDDO                                             
C
      RETURN
      END  
