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
Chd|  S8SLKE3                       source/elements/solid/solide8s/s8slke3.F
Chd|-- called by -----------
Chd|        S8SKE3                        source/elements/solid/solide8s/s8ske3.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        S8SANSB                       source/elements/solid/solide8s/s8sansb.F
Chd|====================================================================
      SUBROUTINE S8SLKE3(
     1   KL,      HH,      VOL,     !DN_X,
     2   DN_Y,    DN_Z,    KSI,     ETA,
     3   ZETA,    DN_R,    DN_S,    DN_T,
     4   INVJ,    X1,      X2,      X3,
     5   X4,      X5,      X6,      X7,
     6   X8,      Y1,      Y2,      Y3,
     7   Y4,      Y5,      Y6,      Y7,
     8   Y8,      Z1,      Z2,      Z3,
     9   Z4,      Z5,      Z6,      Z7,
     A   Z8,      A11,     A12,     A13,
     B   A21,     A22,     A23,     A31,
     C   A32,     A33,     NEL)
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
      INTEGER, INTENT(IN) :: NEL
      my_real
     .   VOL(MVSIZ),KSI,ETA,ZETA
      DOUBLE PRECISION
     .   KL(24,24,MVSIZ), HH(2,MVSIZ), 
     .   DN_X(MVSIZ,8),DN_Y(MVSIZ,8),DN_Z(MVSIZ,8),KTEMP(6,24),B(6,24),
     .   HV(6,6),DN_R(8),DN_S(8),DN_T(8),INVJ(9,MVSIZ)!,KLL(24,24)
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   X5(MVSIZ), X6(MVSIZ), X7(MVSIZ), X8(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Y5(MVSIZ), Y6(MVSIZ), Y7(MVSIZ), Y8(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   Z5(MVSIZ), Z6(MVSIZ), Z7(MVSIZ), Z8(MVSIZ) 
       my_real
     .   A11(MVSIZ), A12(MVSIZ), A13(MVSIZ), 
     .   A21(MVSIZ), A22(MVSIZ), A23(MVSIZ), 
     .   A31(MVSIZ), A32(MVSIZ), A33(MVSIZ)      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#if defined(MYREAL8) && !defined(WITHOUT_LINALG)
      INTEGER I, J,JJ,K,JK, IS
      B(1:6,1:24) = ZERO
      HV(1:6,1:6) = ZERO

      DO I=1,NEL
! Full integration
       !DO J=1,8
       ! JJ=3*(J-1)
       ! B(1,JJ+1) = DN_X(I,J)
       ! B(2,JJ+2) = DN_Y(I,J)
       ! B(3,JJ+3) = DN_Z(I,J)
       ! B(4,JJ+1) = DN_Y(I,J)
       ! B(4,JJ+2) = DN_X(I,J)
       ! B(5,JJ+1) = DN_Z(I,J)
       ! B(5,JJ+3) = DN_X(I,J)
       ! B(6,JJ+2) = DN_Z(I,J)
       ! B(6,JJ+3) = DN_Y(I,J)
       !ENDDO
! ANS formulation
       CALL S8SANSB(
     .    X1(I),X2(I),X3(I),X4(I),X5(I),X6(I),X7(I),X8(I),
     .    Y1(I),Y2(I),Y3(I),Y4(I),Y5(I),Y6(I),Y7(I),Y8(I),                                                                
     .    Z1(I),Z2(I),Z3(I),Z4(I),Z5(I),Z6(I),Z7(I),Z8(I), 
     .    INVJ(1,I),KSI,ETA,ZETA,
     .    A11(I),A12(I),A13(I),
     .    A21(I),A22(I),A23(I),
     .    A31(I),A32(I),A33(I),
     .    DN_R, DN_S, DN_T,B)
       
       HV(1,1) = (HH(1,I)+TWO*HH(2,I))*VOL(I)
       HV(2,1) = HH(1,I)*VOL(I)
       HV(3,1) = HH(1,I)*VOL(I)
       HV(1,2) = HH(1,I)*VOL(I)
       HV(2,2) = (HH(1,I)+TWO*HH(2,I))*VOL(I)
       HV(3,2) = HH(1,I)*VOL(I)
       HV(1,3) = HH(1,I)*VOL(I)
       HV(2,3) = HH(1,I)*VOL(I)
       HV(3,3) = (HH(1,I)+TWO*HH(2,I))*VOL(I)
       HV(4,4) = HH(2,I)*VOL(I)
       HV(5,5) = HH(2,I)*VOL(I)
       HV(6,6) = HH(2,I)*VOL(I)
       
       KTEMP(1:6,1:24)=ZERO
       !DO J=1,6
       ! DO K=1,24
       !  DO JK=1,6
       !   KTEMP(J,K) = KTEMP(J,K)+HV(J,JK)*B(JK,K)
       !  ENDDO
       ! ENDDO
       !ENDDO

       ! CALL DGEMM('N','N',6,24,6,ONE,HV,6,B,6,ZERO,KTEMP,6)
        CALL DSYMM('L','U',6,24,ONE,HV,6,B,6,ZERO,KTEMP,6)
       !DO J=1,24
       ! DO K=1,24
       !  DO JK=1,6
       !   KL(J,K,I) = KL(J,K,I)+B(JK,J)*KTEMP(JK,K)
       !  ENDDO
       ! ENDDO
       !ENDDO
       CALL DGEMM('T','N',24,24,6,ONE,B,6,KTEMP,6,ONE,KL(1,1,I),24)
      ENDDO
#else
      CALL ARRET(5)
#endif
      RETURN
      END
