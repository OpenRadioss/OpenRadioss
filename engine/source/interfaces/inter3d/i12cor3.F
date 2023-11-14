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
Chd|  I12COR3                       source/interfaces/inter3d/i12cor3.F
Chd|-- called by -----------
Chd|        INTTI12A                      source/interfaces/interf/intti12.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I12COR3(
     1   XS,      XM,      IRECT,   MSR,
     2   IRTL,    ILEV,    X,       TETS,
     3   IX1,     IX2,     IX3,     IX4,
     4   X1,      X2,      X3,      X4,
     5   XI,      Y1,      Y2,      Y3,
     6   Y4,      YI,      Z1,      Z2,
     7   Z3,      Z4,      ZI,      XFACE,
     8   LFT,     LLT,     NFT)
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
      INTEGER, INTENT(INOUT) :: NFT
      INTEGER IRECT(4,*), MSR(*), IRTL(*),ILEV
      my_real
     .   XS(3,*),XM(3,*),X(3,*),TETS(*)
      INTEGER, DIMENSION(MVSIZ), INTENT(OUT) ::  IX1,IX2,IX3,IX4
      my_real, DIMENSION(MVSIZ), INTENT(OUT) ::  X1,X2,X3,X4,XI
      my_real, DIMENSION(MVSIZ), INTENT(OUT) ::  Y1,Y2,Y3,Y4,YI
      my_real, DIMENSION(MVSIZ), INTENT(OUT) ::  Z1,Z2,Z3,Z4,ZI
      my_real, DIMENSION(MVSIZ), INTENT(OUT) ::  XFACE
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IL, IG, L, NN
      my_real
     .   DEUPI,TETMIN,TETMAX,RMIN
C-----------------------------------------------

      DEUPI=TWO*PI

      DO I=LFT,LLT
        XFACE(I)=ONE
        IL=I+NFT
        L=IRTL(IL)

        XI(I)=XS(1,IL)
        YI(I)=XS(2,IL)
        ZI(I)=XS(3,IL)

        NN=IRECT(1,L)
        IX1(I)=MSR(NN)
        X1(I)=XM(1,NN)
        Y1(I)=XM(2,NN)
        Z1(I)=XM(3,NN)

        NN=IRECT(2,L)
        IX2(I)=MSR(NN)
        X2(I)=XM(1,NN)
        Y2(I)=XM(2,NN)
        Z2(I)=XM(3,NN)

        NN=IRECT(3,L)
        IX3(I)=MSR(NN)
        X3(I)=XM(1,NN)
        Y3(I)=XM(2,NN)
        Z3(I)=XM(3,NN)

        NN=IRECT(4,L)
        IX4(I)=MSR(NN)
        X4(I)=XM(1,NN)
        Y4(I)=XM(2,NN)
        Z4(I)=XM(3,NN)
      ENDDO
      
      IF(ILEV==1)THEN
      !--------------------------------------------------!
      !   CORRECTION DES TET SI A CHEVAL AXE Y NEGATIF   !
      !--------------------------------------------------!
        DO I=LFT,LLT
          TETMIN=MIN(Z1(I),Z2(I),Z3(I),Z4(I),ZI(I))
          TETMAX=MAX(Z1(I),Z2(I),Z3(I),Z4(I),ZI(I))
          IF(TETMAX-TETMIN >= PI)THEN
            IF(Z1(I)<ZERO)Z1(I)=Z1(I)+DEUPI
            IF(Z2(I)<ZERO)Z2(I)=Z2(I)+DEUPI
            IF(Z3(I)<ZERO)Z3(I)=Z3(I)+DEUPI
            IF(Z4(I)<ZERO)Z4(I)=Z4(I)+DEUPI
            IF(ZI(I)<ZERO)ZI(I)=ZI(I)+DEUPI
          ENDIF
        ENDDO

        !--------------------------------------------------!
        !   CAS D'UNE FACETTE CENTRALE                     !
        !--------------------------------------------------!
        DO I=LFT,LLT
          RMIN=MIN(Y1(I),Y2(I),Y3(I),Y4(I))
          IF(YI(I)<RMIN)THEN
            TETMIN=MIN(Z1(I),Z2(I),Z3(I),Z4(I))
            TETMAX=MAX(Z1(I),Z2(I),Z3(I),Z4(I))
            IF(TETMAX-TETMIN >= PI)THEN
              IL=I+NFT
              L=IRTL(IL)
              NN=MSR(IRECT(1,L))
              IX1(I)=NN
              X1(I)=X(1,NN)
              Y1(I)=X(2,NN)
              Z1(I)=X(3,NN)

              NN=MSR(IRECT(2,L))
              IX2(I)=NN
              X2(I)=X(1,NN)
              Y2(I)=X(2,NN)
              Z2(I)=X(3,NN)

              NN=MSR(IRECT(3,L))
              IX3(I)=NN
              X3(I)=X(1,NN)
              Y3(I)=X(2,NN)
              Z3(I)=X(3,NN)

              NN=MSR(IRECT(4,L))
              IX4(I)=NN
              X4(I)=X(1,NN)
              Y4(I)=X(2,NN)
              Z4(I)=X(3,NN)
              TETS(I)=10001.
            ENDIF          
          ENDIF
        ENDDO !I=LFT,LLT
      ENDIF !(ILEV==1)

      RETURN
      END
