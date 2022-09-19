Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  S8SDERI3                      source/elements/solid/solide8s/s8sderi3.F
Chd|-- called by -----------
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8SKE3                        source/elements/solid/solide8s/s8ske3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8SDERI3(OFFG,OFF,VOLDP,NGL,
     .    KSI,ETA,ZETA,WI,
     .    X1,X2,X3,X4,X5,X6,X7,X8,
     .    Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,
     .    Z1,Z2,Z3,Z4,Z5,Z6,Z7,Z8,
     .    A11,A12,A13,A21,A22,A23,A31,A32,A33,
     .    DN_R,DN_S,DN_T,INVJ,
     .    DN_X,DN_Y,DN_Z,VOLN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com06_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      !INTEGER ICP,IDEG(*)
C     REAL
      my_real
     .   OFF(*),OFFG(*),KSI,ETA,ZETA,WI,VOLN(*),
     .   A11(MVSIZ), A12(MVSIZ), A13(MVSIZ),
     .   A21(MVSIZ), A22(MVSIZ), A23(MVSIZ),
     .   A31(MVSIZ), A32(MVSIZ), A33(MVSIZ)
      DOUBLE PRECISION
     .   VOLDP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J ,ICOR,ep
C     REAL
C                                                                     12
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   X5(MVSIZ), X6(MVSIZ), X7(MVSIZ), X8(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Y5(MVSIZ), Y6(MVSIZ), Y7(MVSIZ), Y8(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   Z5(MVSIZ), Z6(MVSIZ), Z7(MVSIZ), Z8(MVSIZ),VOL(MVSIZ)

      DOUBLE PRECISION RI(8),SI(8),TI(8),DN_R(8),DN_S(8),DN_T(8),
     .   DX_R,DX_S,DX_T,DY_R,DY_S,DY_T,DZ_R,DZ_S,DZ_T,DETM1,
     .   DETJ(MVSIZ),INVJ(9,MVSIZ),
     .   DN_X(MVSIZ,8),DN_Y(MVSIZ,8),DN_Z(MVSIZ,8),C(6,6,MVSIZ),DETDP
C-----------------------------------------------

      DATA RI /-1.0, -1.0,  1.0,  1.0,-1.0, -1.0, 1.0, 1.0/
      DATA SI /-1.0, -1.0, -1.0, -1.0, 1.0,  1.0, 1.0, 1.0/
      DATA TI /-1.0,  1.0,  1.0, -1.0,-1.0,  1.0, 1.0,-1.0/

!sb  ksi->t   eta->r   zeta->s
      DO I=1,8
        DN_R(I) = RI(I)*(ONE+KSI*TI(I))*(ONE+ZETA*SI(I))/EIGHT
        DN_S(I) = SI(I)*(ONE+KSI*TI(I))*(ONE+ETA*RI(I))/EIGHT
        DN_T(I) = TI(I)*(ONE+ETA*RI(I))*(ONE+ZETA*SI(I))/EIGHT
      ENDDO
!     Jacobien
      DO I=LFT,LLT
        DX_R = X1(I)*DN_R(1)+X2(I)*DN_R(2)+X3(I)*DN_R(3)+X4(I)*DN_R(4)
     .        +X5(I)*DN_R(5)+X6(I)*DN_R(6)+X7(I)*DN_R(7)+X8(I)*DN_R(8)
        DX_S = X1(I)*DN_S(1)+X2(I)*DN_S(2)+X3(I)*DN_S(3)+X4(I)*DN_S(4)
     .        +X5(I)*DN_S(5)+X6(I)*DN_S(6)+X7(I)*DN_S(7)+X8(I)*DN_S(8)
        DX_T = X1(I)*DN_T(1)+X2(I)*DN_T(2)+X3(I)*DN_T(3)+X4(I)*DN_T(4)
     .        +X5(I)*DN_T(5)+X6(I)*DN_T(6)+X7(I)*DN_T(7)+X8(I)*DN_T(8)

        DY_R = Y1(I)*DN_R(1)+Y2(I)*DN_R(2)+Y3(I)*DN_R(3)+Y4(I)*DN_R(4)
     .        +Y5(I)*DN_R(5)+Y6(I)*DN_R(6)+Y7(I)*DN_R(7)+Y8(I)*DN_R(8)
        DY_S = Y1(I)*DN_S(1)+Y2(I)*DN_S(2)+Y3(I)*DN_S(3)+Y4(I)*DN_S(4)
     .        +Y5(I)*DN_S(5)+Y6(I)*DN_S(6)+Y7(I)*DN_S(7)+Y8(I)*DN_S(8)
        DY_T = Y1(I)*DN_T(1)+Y2(I)*DN_T(2)+Y3(I)*DN_T(3)+Y4(I)*DN_T(4)
     .        +Y5(I)*DN_T(5)+Y6(I)*DN_T(6)+Y7(I)*DN_T(7)+Y8(I)*DN_T(8)

        DZ_R = Z1(I)*DN_R(1)+Z2(I)*DN_R(2)+Z3(I)*DN_R(3)+Z4(I)*DN_R(4)
     .        +Z5(I)*DN_R(5)+Z6(I)*DN_R(6)+Z7(I)*DN_R(7)+Z8(I)*DN_R(8)
        DZ_S = Z1(I)*DN_S(1)+Z2(I)*DN_S(2)+Z3(I)*DN_S(3)+Z4(I)*DN_S(4)
     .        +Z5(I)*DN_S(5)+Z6(I)*DN_S(6)+Z7(I)*DN_S(7)+Z8(I)*DN_S(8)
        DZ_T = Z1(I)*DN_T(1)+Z2(I)*DN_T(2)+Z3(I)*DN_T(3)+Z4(I)*DN_T(4)
     .        +Z5(I)*DN_T(5)+Z6(I)*DN_T(6)+Z7(I)*DN_T(7)+Z8(I)*DN_T(8)

        DETDP  = DX_R*(DY_S*DZ_T-DZ_S*DY_T)
     .           -DX_S*(DY_R*DZ_T-DY_T*DZ_R)
     .           +DX_T*(DY_R*DZ_S-DY_S*DZ_R)
        DETJ(I) = DETDP
        VOLDP(I) = WI*DETDP

        IF (DETJ(I) > ZERO)THEN
          DETM1 = ONE/DETJ(I)

          INVJ(1,I) = (DY_S*DZ_T-DZ_S*DY_T)*DETM1
          INVJ(2,I) = (DZ_R*DY_T-DY_R*DZ_T)*DETM1
          INVJ(3,I) = (DY_R*DZ_S-DY_S*DZ_R)*DETM1
          INVJ(4,I) = (DX_T*DZ_S-DX_S*DZ_T)*DETM1
          INVJ(5,I) = (DX_R*DZ_T-DX_T*DZ_R)*DETM1
          INVJ(6,I) = (DX_S*DZ_R-DX_R*DZ_S)*DETM1
          INVJ(7,I) = (DX_S*DY_T-DX_T*DY_S)*DETM1
          INVJ(8,I) = (DX_T*DY_R-DX_R*DY_T)*DETM1
          INVJ(9,I) = (DX_R*DY_S-DX_S*DY_R)*DETM1
        ELSE

        ENDIF

        A11(I) = DX_T !R
        A12(I) = DY_T !R
        A13(I) = DZ_T !R
        A21(I) = DX_R !S
        A22(I) = DY_R !S
        A23(I) = DZ_R !S
        A31(I) = DX_S !T
        A32(I) = DY_S !T
        A33(I) = DZ_S !T
      ENDDO

      DO J=1,8
        DO I=LFT,LLT
          DN_X(I,J) = DN_R(J)*INVJ(1,I)+DN_S(J)*INVJ(2,I)+DN_T(J)*INVJ(3,I)
          DN_Y(I,J) = DN_R(J)*INVJ(4,I)+DN_S(J)*INVJ(5,I)+DN_T(J)*INVJ(6,I)
          DN_Z(I,J) = DN_R(J)*INVJ(7,I)+DN_S(J)*INVJ(8,I)+DN_T(J)*INVJ(9,I)
        ENDDO
      ENDDO

      ICOR = 0
      DO I=LFT,LLT
        OFF(I)=OFFG(I)
        IF(OFF(I)==ZERO)THEN
         DETJ(I)=ONE
         IF (VOLDP(I)<=ZERO) VOLDP(I)=ONE
        ELSEIF (VOLDP(I)<=ZERO ) THEN
         VOLDP(I)= EM20
         OFF(I) =ZERO
          ICOR=1
        ENDIF
      ENDDO
C
      IF (ICOR>0.AND.IMPL_S>0) THEN
        DO I=LFT,LLT
          IF(VOLDP(I)<=ZERO.AND.OFF(I)/=ZERO)THEN
            VOLDP(I)= EM20
            OFF(I) =ZERO
           IF (IMP_CHK>0) THEN
#include "lockon.inc"
            WRITE(IOUT ,2001) NGL(I)
#include "lockoff.inc"
            IDEL7NOK = 1
            IMP_IR = IMP_IR + 1
           ELSEIF (IMCONV==1) THEN
#include "lockon.inc"
            WRITE(ISTDO,2000) NGL(I)
            WRITE(IOUT ,2000) NGL(I)
#include "lockoff.inc"
            IDEL7NOK = 1
           ENDIF
          ENDIF
        ENDDO
      END IF

      DO I=LFT,LLT
       VOLN(I) = VOLDP(I)
      ENDDO
c

C
 2000 FORMAT(/' ZERO OR NEGATIVE SUB-VOLUME : DELETE 3D-ELEMENT NB',
     .          I10/)
 2001 FORMAT(/' ZERO OR NEGATIVE SOLID SUB-VOLUME : ELEMENT NB:',
     .          I10/)
      RETURN
      END
