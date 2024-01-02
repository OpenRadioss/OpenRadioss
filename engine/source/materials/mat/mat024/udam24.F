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
Chd|  UDAM24                        source/materials/mat/mat024/udam24.F
Chd|-- called by -----------
Chd|        M24ANIM                       source/materials/mat/mat024/m24anim.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UDAM24(SIG,ANG,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real SIG(NEL,6), ANG(NEL,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real ANGA,
     .   S11(NEL), S12(NEL), S13(NEL), S21(NEL), S22(NEL), S23(NEL), S31(NEL),
     .   S32(NEL), S33(NEL), A11(NEL), A12(NEL), A13(NEL), A21(NEL), A22(NEL), 
     .   A23(NEL),A31(NEL), A32(NEL), A33(NEL) 
C=======================================================================
      ANGA = ZERO
      DO I = 1,NEL
        ANGA = ANGA + ABS(ANG(I,1)-ONE) + ABS(ANG(I,5)-ONE)
      ENDDO
      IF (ANGA <= EM3) RETURN
C
      DO5 I = 1,NEL
      S11(I) = ANG(I,1)
      S21(I) = ANG(I,2)
      S31(I) = ANG(I,3)
      S12(I) = ANG(I,4)
      S22(I) = ANG(I,5)
      S32(I) = ANG(I,6)
      S13(I) = S21(I)*S32(I)-S31(I)*S22(I)
      S23(I) = S31(I)*S12(I)-S11(I)*S32(I)
  5   S33(I) = S11(I)*S22(I)-S21(I)*S12(I)
C . . . . . . . . . . . . . . . . . .
C     ROTATION INVERSE DES CONTRAINTES
C . . . . . . . . . . . . . . . . . .
      DO I = 1,NEL
        A11(I)=SIG(I,1)*S11(I)+SIG(I,4)*S12(I)+SIG(I,6)*S13(I)
        A12(I)=SIG(I,1)*S21(I)+SIG(I,4)*S22(I)+SIG(I,6)*S23(I)
        A13(I)=SIG(I,1)*S31(I)+SIG(I,4)*S32(I)+SIG(I,6)*S33(I)
        A21(I)=SIG(I,4)*S11(I)+SIG(I,2)*S12(I)+SIG(I,5)*S13(I)
        A22(I)=SIG(I,4)*S21(I)+SIG(I,2)*S22(I)+SIG(I,5)*S23(I)
        A23(I)=SIG(I,4)*S31(I)+SIG(I,2)*S32(I)+SIG(I,5)*S33(I)
        A31(I)=SIG(I,6)*S11(I)+SIG(I,5)*S12(I)+SIG(I,3)*S13(I)
        A32(I)=SIG(I,6)*S21(I)+SIG(I,5)*S22(I)+SIG(I,3)*S23(I)
        A33(I)=SIG(I,6)*S31(I)+SIG(I,5)*S32(I)+SIG(I,3)*S33(I)
      ENDDO
c
      DO I = 1,NEL
        SIG(I,1)=S11(I)*A11(I)+S12(I)*A21(I)+S13(I)*A31(I)
        SIG(I,2)=S21(I)*A12(I)+S22(I)*A22(I)+S23(I)*A32(I)
        SIG(I,3)=S31(I)*A13(I)+S32(I)*A23(I)+S33(I)*A33(I)
        SIG(I,4)=S11(I)*A12(I)+S12(I)*A22(I)+S13(I)*A32(I)
        SIG(I,5)=S21(I)*A13(I)+S22(I)*A23(I)+S23(I)*A33(I)
        SIG(I,6)=S11(I)*A13(I)+S12(I)*A23(I)+S13(I)*A33(I)
      ENDDO
c-----------
      RETURN
      END


Chd|====================================================================
Chd|  UDAM24N                       source/materials/mat/mat024/udam24.F
Chd|-- called by -----------
Chd|        CONC24                        source/materials/mat/mat024/conc24.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UDAM24N(SIG,ANG,NBDAMA,DAMAI,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NBDAMA,NEL
      INTEGER DAMAI(NEL)
      my_real SIG(NEL,6), ANG(NEL,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K
      my_real ANGA,
     .   S11(NEL), S12(NEL), S13(NEL), S21(NEL), S22(NEL), S23(NEL), S31(NEL),
     .   S32(NEL), S33(NEL), A11(NEL), A12(NEL), A13(NEL), A21(NEL), A22(NEL), 
     .   A23(NEL), A31(NEL), A32(NEL), A33(NEL)
C=======================================================================
      DO 5 K=1,NBDAMA
      I = DAMAI(K)
      S11(I)=ANG(I,1)
      S21(I)=ANG(I,2)
      S31(I)=ANG(I,3)
      S12(I)=ANG(I,4)
      S22(I)=ANG(I,5)
      S32(I)=ANG(I,6)
      S13(I)=S21(I)*S32(I)-S31(I)*S22(I)
      S23(I)=S31(I)*S12(I)-S11(I)*S32(I)
  5   S33(I)=S11(I)*S22(I)-S21(I)*S12(I)
C . . . . . . . . . . . . . . . . . .
C     ROTATION INVERSE DES CONTRAINTES
C . . . . . . . . . . . . . . . . . .
      DO K=1,NBDAMA
      I = DAMAI(K)
      A11(I)=SIG(I,1)*S11(I)+SIG(I,4)*S12(I)+SIG(I,6)*S13(I)
      A12(I)=SIG(I,1)*S21(I)+SIG(I,4)*S22(I)+SIG(I,6)*S23(I)
      A13(I)=SIG(I,1)*S31(I)+SIG(I,4)*S32(I)+SIG(I,6)*S33(I)
      A21(I)=SIG(I,4)*S11(I)+SIG(I,2)*S12(I)+SIG(I,5)*S13(I)
      A22(I)=SIG(I,4)*S21(I)+SIG(I,2)*S22(I)+SIG(I,5)*S23(I)
      A23(I)=SIG(I,4)*S31(I)+SIG(I,2)*S32(I)+SIG(I,5)*S33(I)
      A31(I)=SIG(I,6)*S11(I)+SIG(I,5)*S12(I)+SIG(I,3)*S13(I)
      A32(I)=SIG(I,6)*S21(I)+SIG(I,5)*S22(I)+SIG(I,3)*S23(I)
      A33(I)=SIG(I,6)*S31(I)+SIG(I,5)*S32(I)+SIG(I,3)*S33(I)
      ENDDO
c
      DO K=1,NBDAMA
      I = DAMAI(K)
      SIG(I,1)=S11(I)*A11(I)+S12(I)*A21(I)+S13(I)*A31(I)
      SIG(I,2)=S21(I)*A12(I)+S22(I)*A22(I)+S23(I)*A32(I)
      SIG(I,3)=S31(I)*A13(I)+S32(I)*A23(I)+S33(I)*A33(I)
      SIG(I,4)=S11(I)*A12(I)+S12(I)*A22(I)+S13(I)*A32(I)
      SIG(I,5)=S21(I)*A13(I)+S22(I)*A23(I)+S23(I)*A33(I)
      SIG(I,6)=S11(I)*A13(I)+S12(I)*A23(I)+S13(I)*A33(I)
      ENDDO
c-----------
      RETURN
      END
