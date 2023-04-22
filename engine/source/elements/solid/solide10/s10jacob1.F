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
Chd|  S10JACOB1                     source/elements/solid/solide10/s10jacob1.F
Chd|-- called by -----------
Chd|        S10DERI3                      source/elements/solid/solide10/s10deri3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10JACOB1(ALPH,BETA,W,
     .   X1B,X2B,X3B,X4A,X5B,X6B,X7B,X8B,X9B,X10B,X8A,X9A,X10A,
     .   Y1B,Y2B,Y3B,Y4A,Y5B,Y6B,Y7B,Y8B,Y9B,Y10B,Y8A,Y9A,Y10A,
     .   Z1B,Z2B,Z3B,Z4A,Z5B,Z6B,Z7B,Z8B,Z9B,Z10B,Z8A,Z9A,Z10A,
     .   PX1,PX2,PX3,PX4,PX5,PX6,PX7,PX8,PX9,PX10,
     .   PY1,PY2,PY3,PY4,PY5,PY6,PY7,PY8,PY9,PY10,
     .   PZ1,PZ2,PZ3,PZ4,PZ5,PZ6,PZ7,PZ8,PZ9,PZ10,
     .   NX1,NX2,NX3,NX4,NX5,NX6,NX7,NX8,NX9,NX10,
     .   VOL,NNEGA,INDEX,VOLDP)
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
      INTEGER NNEGA,INDEX(*)
C     REAL
       DOUBLE PRECISION
     .   X1B(MVSIZ),X2B(MVSIZ),X3B(MVSIZ),X4A(MVSIZ),X5B(MVSIZ),
     .   X6B(MVSIZ),X7B(MVSIZ),X8B(MVSIZ),X9B(MVSIZ),X10B(MVSIZ),
     .   X8A(MVSIZ),X9A(MVSIZ),X10A(MVSIZ),
     .   Y1B(MVSIZ),Y2B(MVSIZ),Y3B(MVSIZ),Y4A(MVSIZ),Y5B(MVSIZ),
     .   Y6B(MVSIZ),Y7B(MVSIZ),Y8B(MVSIZ),Y9B(MVSIZ),Y10B(MVSIZ),
     .   Y8A(MVSIZ),Y9A(MVSIZ),Y10A(MVSIZ),
     .   Z1B(MVSIZ),Z2B(MVSIZ),Z3B(MVSIZ),Z4A(MVSIZ),Z5B(MVSIZ),
     .   Z6B(MVSIZ),Z7B(MVSIZ),Z8B(MVSIZ),Z9B(MVSIZ),Z10B(MVSIZ),
     .   Z8A(MVSIZ),Z9A(MVSIZ),Z10A(MVSIZ),VOLDP(MVSIZ)

       my_real
     .   PX1(MVSIZ),PX2(MVSIZ),PX3(MVSIZ),PX4(MVSIZ),PX5(MVSIZ),
     .   PX6(MVSIZ),PX7(MVSIZ),PX8(MVSIZ),PX9(MVSIZ),PX10(MVSIZ),
     .   PY1(MVSIZ),PY2(MVSIZ),PY3(MVSIZ),PY4(MVSIZ),PY5(MVSIZ),
     .   PY6(MVSIZ),PY7(MVSIZ),PY8(MVSIZ),PY9(MVSIZ),PY10(MVSIZ),
     .   PZ1(MVSIZ),PZ2(MVSIZ),PZ3(MVSIZ),PZ4(MVSIZ),PZ5(MVSIZ),
     .   PZ6(MVSIZ),PZ7(MVSIZ),PZ8(MVSIZ),PZ9(MVSIZ),PZ10(MVSIZ),
     .   NX1(MVSIZ),NX2(MVSIZ),NX3(MVSIZ),NX4(MVSIZ),NX5(MVSIZ),
     .   NX6(MVSIZ),NX7(MVSIZ),NX8(MVSIZ),NX9(MVSIZ),NX10(MVSIZ),
     .   VOL(MVSIZ),ALPH,BETA,W
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      DOUBLE PRECISION 
     .   D,A4MB4,A4,B4,BB,AB,B2,
     .   DXDR,DXDS,DXDT,DYDR,DYDS,DYDT,DZDR,DZDS,DZDT
     
      DOUBLE PRECISION 
     .   AA,A4M1(MVSIZ),B4M1(MVSIZ)
      
      DOUBLE PRECISION 
     .   DRDX(MVSIZ), DSDX(MVSIZ), DTDX(MVSIZ),
     .   DRDY(MVSIZ), DSDY(MVSIZ), DTDY(MVSIZ),
     .   DRDZ(MVSIZ), DSDZ(MVSIZ), DTDZ(MVSIZ),
     .   DET(MVSIZ)
C-----------------------------------------------
c      AA = (TWO*ALPH - ONE)*ALPH
c      BB = (TWO*BETA - ONE)*BETA
c      B2 = FOUR*BETA*BETA
c      AB = FOUR*ALPH*BETA      
C-----------------------------------------------
#include "vectorize.inc"
      DO J=1,NNEGA
       I = INDEX(J)
       AA = X5B(I) + X6B(I) + X7B(I)
     .    - X4A(I) - X8B(I) - X9B(I) - X10B(I)
       DXDR = X1B(I) +X8A(I) - X6B(I) + AA
       DXDS = X2B(I) +X9A(I) - X7B(I) + AA
       DXDT = X3B(I) +X10A(I)- X5B(I) + AA
C
       AA = Y5B(I) + Y6B(I) + Y7B(I)
     .    - Y4A(I) - Y8B(I) - Y9B(I) - Y10B(I)
       DYDR = Y1B(I) +Y8A(I) - Y6B(I) + AA
       DYDS = Y2B(I) +Y9A(I) - Y7B(I) + AA
       DYDT = Y3B(I) +Y10A(I)- Y5B(I) + AA
C
       AA = Z5B(I) + Z6B(I) + Z7B(I)
     .    - Z4A(I) - Z8B(I) - Z9B(I) - Z10B(I)
       DZDR = Z1B(I) +Z8A(I) - Z6B(I) + AA
       DZDS = Z2B(I) +Z9A(I) - Z7B(I) + AA
       DZDT = Z3B(I) +Z10A(I)- Z5B(I) + AA
C
       DRDX(I)=DYDS*DZDT-DZDS*DYDT
       DSDX(I)=DYDT*DZDR-DZDT*DYDR
       DTDX(I)=DYDR*DZDS-DZDR*DYDS
C
       DRDY(I)=DZDS*DXDT-DXDS*DZDT
       DSDY(I)=DZDT*DXDR-DXDT*DZDR
       DTDY(I)=DZDR*DXDS-DXDR*DZDS
C
       DRDZ(I)=DXDS*DYDT-DYDS*DXDT
       DSDZ(I)=DXDT*DYDR-DYDT*DXDR
       DTDZ(I)=DXDR*DYDS-DYDR*DXDS
C
       DET(I) = DXDR * DRDX(I)
     .        + DYDR * DRDY(I)
     .        + DZDR * DRDZ(I)
       VOLDP(I) = W * DET(I) * ONE_OVER_6
       VOL(I) = VOLDP(I)
C
      ENDDO
C      
#include "vectorize.inc"
      DO J=1,NNEGA
       I = INDEX(J)
        D = ONE/MAX(EM30,DET(I))
C
        A4 = FOUR * ALPH
        B4 = FOUR * BETA
        A4M1(I)  = A4 - ONE
        B4M1(I)  = B4 - ONE
        B4M1(I) = D * B4M1(I)
        A4M1(I) = D * A4M1(I)
        B4 = D * B4
        A4 = D * A4
        A4MB4  = A4 - B4
C
        PX1(I) = B4M1(I) * DRDX(I)
        PY1(I) = B4M1(I) * DRDY(I)
        PZ1(I) = B4M1(I) * DRDZ(I)
C
        PX2(I) = B4M1(I) * DSDX(I)
        PY2(I) = B4M1(I) * DSDY(I)
        PZ2(I) = B4M1(I) * DSDZ(I)
C
        PX3(I) = B4M1(I) * DTDX(I)
        PY3(I) = B4M1(I) * DTDY(I)
        PZ3(I) = B4M1(I) * DTDZ(I)
C
        PX4(I) =-A4M1(I) * (DRDX(I)+DSDX(I)+DTDX(I))
        PY4(I) =-A4M1(I) * (DRDY(I)+DSDY(I)+DTDY(I))
        PZ4(I) =-A4M1(I) * (DRDZ(I)+DSDZ(I)+DTDZ(I))
C
        PX5(I) = B4 * (DRDX(I)+DSDX(I))
        PY5(I) = B4 * (DRDY(I)+DSDY(I))
        PZ5(I) = B4 * (DRDZ(I)+DSDZ(I))
C
        PX6(I) = B4 * (DSDX(I)+DTDX(I))
        PY6(I) = B4 * (DSDY(I)+DTDY(I))
        PZ6(I) = B4 * (DSDZ(I)+DTDZ(I))
C
        PX7(I) = B4 * (DTDX(I)+DRDX(I))
        PY7(I) = B4 * (DTDY(I)+DRDY(I))
        PZ7(I) = B4 * (DTDZ(I)+DRDZ(I))
C
        PX8(I) = A4MB4  * DRDX(I) - PX6(I)
        PY8(I) = A4MB4  * DRDY(I) - PY6(I)
        PZ8(I) = A4MB4  * DRDZ(I) - PZ6(I)
C
        PX9(I) = A4MB4  * DSDX(I) - PX7(I)
        PY9(I) = A4MB4  * DSDY(I) - PY7(I)
        PZ9(I) = A4MB4  * DSDZ(I) - PZ7(I)
C
        PX10(I)= A4MB4  * DTDX(I) - PX5(I)
        PY10(I)= A4MB4  * DTDY(I) - PY5(I)
        PZ10(I)= A4MB4  * DTDZ(I) - PZ5(I)
C
      ENDDO
C
      RETURN
      END
