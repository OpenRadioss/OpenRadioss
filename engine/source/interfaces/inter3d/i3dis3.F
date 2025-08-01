Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    i3dis3    ../engine/source/interfaces/inter3d/i3dis3.F
!||--- called by ------------------------------------------------------
!||    i5ke3     ../engine/source/interfaces/inter3d/i5ke3.F
!||    i6main    ../engine/source/interfaces/inter3d/i6main.F
!||    i9main3   ../engine/source/interfaces/int09/i9main3.F
!||    intvo3    ../engine/source/interfaces/inter3d/intvo3.F
!||    ud_int5   ../engine/source/implicit/upd_glob_k.F
!||====================================================================
      SUBROUTINE I3DIS3(
     1   IGIMP,   NTY,     DIST,    X1,
     2   X2,      X3,      X4,      XI,
     3   Y1,      Y2,      Y3,      Y4,
     4   YI,      Z1,      Z2,      Z3,
     5   Z4,      ZI,      XFACE,   N1,
     6   N2,      N3,      SSC,     TTC,
     7   ALP,     ANS,     XP,      YP,
     8   ZP,      H1,      H2,      H3,
     9   H4,      LFT,     LLT)
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
      INTEGER, INTENT(INOUT) :: LFT
      INTEGER, INTENT(INOUT) :: LLT
      INTEGER IGIMP,NTY
      my_real DIST(*)
      my_real, DIMENSION(MVSIZ), INTENT(IN) ::  X1,X2,X3,X4,XI
      my_real, DIMENSION(MVSIZ), INTENT(IN) ::  Y1,Y2,Y3,Y4,YI
      my_real, DIMENSION(MVSIZ), INTENT(IN) ::  Z1,Z2,Z3,Z4,ZI
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) ::  XFACE,ANS
      my_real, DIMENSION(MVSIZ), INTENT(IN) ::  N1,N2,N3,ALP
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: XP,YP,ZP,SSC,TTC
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: H1,H2,H3,H4
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
      DO I=LFT,LLT
        IF (XFACE(I) == ZERO) CYCLE
        IF (ABS(SSC(I))>ONE+ALP(I) .OR. ABS(TTC(I))>ONE+ALP(I)) THEN
          XFACE(I)=ZERO
        ELSE
          IF(ABS(SSC(I)) > ONE) SSC(I)=SSC(I)/ABS(SSC(I))
          IF(ABS(TTC(I)) > ONE) TTC(I)=TTC(I)/ABS(TTC(I))
        ENDIF
      END DO
C
      IGIMP=0
      DO I=LFT,LLT
        IGIMP = IGIMP + ABS(XFACE(I))
      END DO
      IF (IGIMP == 0) RETURN
C
      DO I=LFT,LLT
        H1(I) = FOURTH*(ONE-TTC(I))*(ONE-SSC(I))
        H2(I) = FOURTH*(ONE-TTC(I))*(ONE+SSC(I))
        H3(I) = FOURTH*(ONE+TTC(I))*(ONE+SSC(I))
        H4(I) = FOURTH*(ONE+TTC(I))*(ONE-SSC(I))
      END DO
C
      DO I=LFT,LLT
        XP(I)=H1(I)*X1(I)+H2(I)*X2(I)+H3(I)*X3(I)+H4(I)*X4(I)
        YP(I)=H1(I)*Y1(I)+H2(I)*Y2(I)+H3(I)*Y3(I)+H4(I)*Y4(I)
        ZP(I)=H1(I)*Z1(I)+H2(I)*Z2(I)+H3(I)*Z3(I)+H4(I)*Z4(I)
      END DO
C
      DO I=LFT,LLT
        ANS(I)= N1(I)*(XI(I)-XP(I))
     .         +N2(I)*(YI(I)-YP(I))
     .         +N3(I)*(ZI(I)-ZP(I))
      END DO
      IF (NTY == 8) THEN
        DO I=LFT,LLT
          DIST(I) = ANS(I)
        ENDDO
      ENDIF
C-----------
      RETURN
      END
