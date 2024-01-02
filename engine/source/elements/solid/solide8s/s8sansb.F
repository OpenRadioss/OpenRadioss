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
Chd|  S8SANSB                       source/elements/solid/solide8s/s8sansb.F
Chd|-- called by -----------
Chd|        S8SLKE3                       source/elements/solid/solide8s/s8slke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8SANSB(
     .    X1,X2,X3,X4,X5,X6,X7,X8,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,
     .    Z1,Z2,Z3,Z4,Z5,Z6,Z7,Z8, INVJ,KSI,ETA,ZETA,
     .    A11,A12,A13,A21,A22,A23,A31,A32,A33,
     .    DN_R, DN_S, DN_T,BB)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   DN_R(8), DN_S(8), DN_T(8), INVJ(9),
     .   X1,X2,X3,X4,X5,X6,X7,X8,
     .   Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,
     .   Z1,Z2,Z3,Z4,Z5,Z6,Z7,Z8,
     .   A11, A12, A13, 
     .   A21, A22, A23, 
     .   A31, A32, A33, 
     .   KSI,ETA,ZETA, BB(6,24)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,JJ,L,M,LM
C     REAL
      my_real
     .   B(6,24),C(6,6),UM,UP,
     .   A1A1,A1A2,A1A3,A2A1,A2A2,A2A3,
     .   A1B1,A1B2,A1B3,A2B1,A2B2,A2B3,
     .   B1A1,B1A2,B1A3,B2A1,B2A2,B2A3,
     .   B1B1,B1B2,B1B3,B2B1,B2B2,B2B3,
     .   C1A1,C1A2,C1A3,C2A1,C2A2,C2A3,
     .   C1B1,C1B2,C1B3,C2B1,C2B2,C2B3,
     .   CA11,CA21,CA31,CA12,CA22,CA32,CA13,CA23,CA33
C                                                                     
C-----------------------------------------------
        DO J=1,8
         JJ = 3*(J-1)
         B(1,JJ+1) =  A11 *DN_T(J)
         B(1,JJ+2) =  A12 *DN_T(J)
         B(1,JJ+3) =  A13 *DN_T(J)
         B(2,JJ+1) =  A21 *DN_R(J)
         B(2,JJ+2) =  A22 *DN_R(J)
         B(2,JJ+3) =  A23 *DN_R(J)
         B(3,JJ+1) =  A31 *DN_S(J)
         B(3,JJ+2) =  A32 *DN_S(J)
         B(3,JJ+3) =  A33 *DN_S(J)
        ENDDO
! sortir cette partie car independante du PG
        A1A1 = (X2 +X3 -X1 -X4 )*FOURTH
        A1A2 = (Y2 +Y3 -Y1 -Y4 )*FOURTH
        A1A3 = (Z2 +Z3 -Z1 -Z4 )*FOURTH
        A2A1 = (X4 +X3 -X1 -X2 )*FOURTH
        A2A2 = (Y4 +Y3 -Y1 -Y2 )*FOURTH
        A2A3 = (Z4 +Z3 -Z1 -Z2 )*FOURTH
        A1B1 = (X6 +X7 -X5 -X8 )*FOURTH
        A1B2 = (Y6 +Y7 -Y5 -Y8 )*FOURTH
        A1B3 = (Z6 +Z7 -Z5 -Z8 )*FOURTH
        A2B1 = (X8 +X7 -X5 -X6 )*FOURTH
        A2B2 = (Y8 +Y7 -Y5 -Y6 )*FOURTH
        A2B3 = (Z8 +Z7 -Z5 -Z6 )*FOURTH
!
        UM = (ONE-ZETA)/EIGHT
        UP = (ONE+ZETA)/EIGHT
        B(4,1) = -UM*(A1A1+A2A1)
        B(4,2) = -UM*(A1A2+A2A2)
        B(4,3) = -UM*(A1A3+A2A3)
        B(4,4) =  UM*(A2A1-A1A1)
        B(4,5) =  UM*(A2A2-A1A2)
        B(4,6) =  UM*(A2A3-A1A3)
        B(4,7) =  UM*(A2A1+A1A1)
        B(4,8) =  UM*(A2A2+A1A2)
        B(4,9) =  UM*(A2A3+A1A3)
        B(4,10)=  UM*(A1A1-A2A1)
        B(4,11)=  UM*(A1A2-A2A2)
        B(4,12)=  UM*(A1A3-A2A3)
        B(4,13) =-UP*(A1B1+A2B1)
        B(4,14) =-UP*(A1B2+A2B2)
        B(4,15) =-UP*(A1B3+A2B3)
        B(4,16) = UP*(A2B1-A1B1)
        B(4,17) = UP*(A2B2-A1B2)
        B(4,18) = UP*(A2B3-A1B3)
        B(4,19) = UP*(A2B1+A1B1)
        B(4,20) = UP*(A2B2+A1B2)
        B(4,21) = UP*(A2B3+A1B3)
        B(4,22) = UP*(A1B1-A2B1)
        B(4,23) = UP*(A1B2-A2B2)
        B(4,24) = UP*(A1B3-A2B3)
      
! sortir cette partie car independante du PG
        B1A1 = (X2 +X6 -X1 -X5 )*FOURTH
        B1A2 = (Y2 +Y6 -Y1 -Y5 )*FOURTH
        B1A3 = (Z2 +Z6 -Z1 -Z5 )*FOURTH
        B2A1 = (X5 +X6 -X1 -X2 )*FOURTH
        B2A2 = (Y5 +Y6 -Y1 -Y2 )*FOURTH
        B2A3 = (Z5 +Z6 -Z1 -Z2 )*FOURTH
        B1B1 = (X3 +X7 -X4 -X8 )*FOURTH
        B1B2 = (Y3 +Y7 -Y4 -Y8 )*FOURTH
        B1B3 = (Z3 +Z7 -Z4 -Z8 )*FOURTH
        B2B1 = (X7 +X8 -X3 -X4 )*FOURTH
        B2B2 = (Y7 +Y8 -Y3 -Y4 )*FOURTH
        B2B3 = (Z7 +Z8 -Z3 -Z4 )*FOURTH
!
        UM = (ONE-ETA)/EIGHT
        UP = (ONE+ETA)/EIGHT
        B(5,1) = -UM*(B1A1+B2A1)
        B(5,2) = -UM*(B1A2+B2A2)
        B(5,3) = -UM*(B1A3+B2A3)
        B(5,4) =  UM*(B2A1-B1A1)
        B(5,5) =  UM*(B2A2-B1A2)
        B(5,6) =  UM*(B2A3-B1A3)
        B(5,7) =  UP*(B2B1-B1B1)
        B(5,8) =  UP*(B2B2-B1B2)
        B(5,9) =  UP*(B2B3-B1B3)
        B(5,10)= -UP*(B1B1+B2B1)
        B(5,11)= -UP*(B1B2+B2B2)
        B(5,12)= -UP*(B1B3+B2B3)
        B(5,13)=  UM*(B1A1-B2A1)
        B(5,14)=  UM*(B1A2-B2A2)
        B(5,15)=  UM*(B1A3-B2A3)
        B(5,16)=  UM*(B1A1+B2A1)
        B(5,17)=  UM*(B1A2+B2A2)
        B(5,18)=  UM*(B1A3+B2A3)
        B(5,19)=  UP*(B1B1+B2B1)
        B(5,20)=  UP*(B1B2+B2B2)
        B(5,21)=  UP*(B1B3+B2B3)
        B(5,22)=  UP*(B1B1-B2B1)
        B(5,23)=  UP*(B1B2-B2B2)
        B(5,24)=  UP*(B1B3-B2B3)
      
! sortir cette partie car independante du PG
        C1A1 = (X4 +X8 -X1 -X5 )*FOURTH
        C1A2 = (Y4 +Y8 -Y1 -Y5 )*FOURTH
        C1A3 = (Z4 +Z8 -Z1 -Z5 )*FOURTH
        C2A1 = (X5 +X8 -X1 -X4 )*FOURTH
        C2A2 = (Y5 +Y8 -Y1 -Y4 )*FOURTH
        C2A3 = (Z5 +Z8 -Z1 -Z4 )*FOURTH
        C1B1 = (X3 +X7 -X2 -X6 )*FOURTH
        C1B2 = (Y3 +Y7 -Y2 -Y6 )*FOURTH
        C1B3 = (Z3 +Z7 -Z2 -Z6 )*FOURTH
        C2B1 = (X6 +X7 -X2 -X3 )*FOURTH
        C2B2 = (Y6 +Y7 -Y2 -Y3 )*FOURTH
        C2B3 = (Z6 +Z7 -Z2 -Z3 )*FOURTH
!
        UM = (ONE-KSI)/EIGHT
        UP = (ONE+KSI)/EIGHT
        B(6,1) = -UM*(C1A1+C2A1)
        B(6,2) = -UM*(C1A2+C2A2)
        B(6,3) = -UM*(C1A3+C2A3)
        B(6,4) = -UP*(C1B1+C2B1)
        B(6,5) = -UP*(C1B2+C2B2)
        B(6,6) = -UP*(C1B3+C2B3)
        B(6,7) =  UP*(C2B1-C1B1)
        B(6,8) =  UP*(C2B2-C1B2)
        B(6,9) =  UP*(C2B3-C1B3)
        B(6,10)=  UM*(C2A1-C1A1)
        B(6,11)=  UM*(C2A2-C1A2)
        B(6,12)=  UM*(C2A3-C1A3)
        B(6,13)=  UM*(C1A1-C2A1)
        B(6,14)=  UM*(C1A2-C2A2)
        B(6,15)=  UM*(C1A3-C2A3)
        B(6,16)=  UP*(C1B1-C2B1)
        B(6,17)=  UP*(C1B2-C2B2)
        B(6,18)=  UP*(C1B3-C2B3)
        B(6,19)=  UP*(C1B1+C2B1)
        B(6,20)=  UP*(C1B2+C2B2)
        B(6,21)=  UP*(C1B3+C2B3)
        B(6,22)=  UM*(C1A1+C2A1)
        B(6,23)=  UM*(C1A2+C2A2)
        B(6,24)=  UM*(C1A3+C2A3)      

        CA11 = INVJ(3)
        CA21 = INVJ(1)
        CA31 = INVJ(2)
        CA12 = INVJ(6)
        CA22 = INVJ(4)
        CA32 = INVJ(5)
        CA13 = INVJ(9)
        CA23 = INVJ(7)
        CA33 = INVJ(8)
       
        C(1,1) = CA11*CA11
        C(2,1) = CA12*CA12
        C(3,1) = CA13*CA13
        C(4,1) = TWO*CA11*CA12
        C(5,1) = TWO*CA11*CA13
        C(6,1) = TWO*CA12*CA13
        C(1,2) = CA21*CA21
        C(2,2) = CA22*CA22
        C(3,2) = CA23*CA23
        C(4,2) = TWO*CA21*CA22
        C(5,2) = TWO*CA21*CA23
        C(6,2) = TWO*CA22*CA23
        C(1,3) = CA31*CA31
        C(2,3) = CA32*CA32
        C(3,3) = CA33*CA33
        C(4,3) = TWO*CA31*CA32
        C(5,3) = TWO*CA31*CA33
        C(6,3) = TWO*CA32*CA33
        C(1,4) = CA11*CA21
        C(2,4) = CA12*CA22
        C(3,4) = CA13*CA23
        C(4,4) = CA12*CA21+CA11*CA22
        C(5,4) = CA13*CA21+CA11*CA23
        C(6,4) = CA12*CA23+CA13*CA22
        C(1,5) = CA11*CA31
        C(2,5) = CA12*CA32
        C(3,5) = CA13*CA33
        C(4,5) = CA12*CA31+CA11*CA32
        C(5,5) = CA13*CA31+CA11*CA33
        C(6,5) = CA12*CA33+CA13*CA32
        C(1,6) = CA21*CA31
        C(2,6) = CA22*CA32
        C(3,6) = CA23*CA33
        C(4,6) = CA22*CA31+CA21*CA32
        C(5,6) = CA23*CA31+CA21*CA33
        C(6,6) = CA23*CA32+CA22*CA33
        
        DO L=1,6
         DO M=1,24
          BB(L,M) = ZERO
          DO LM=1,6
           BB(L,M) = BB(L,M)+C(L,LM)*B(LM,M)
          ENDDO
         ENDDO
        ENDDO
C
      RETURN
      END
