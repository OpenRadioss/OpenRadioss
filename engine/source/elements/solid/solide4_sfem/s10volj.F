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
Chd|  S10VOLJ                       source/elements/solid/solide4_sfem/s10volj.F
Chd|-- called by -----------
Chd|        S10VOLNOD3                    source/elements/solid/solide4_sfem/s10volnod3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10VOLJ(W,
     .   X1B,X2B,X3B,X4A,X5B,X6B,X7B,X8B,X9B,X10B,X8A,X9A,X10A,
     .   Y1B,Y2B,Y3B,Y4A,Y5B,Y6B,Y7B,Y8B,Y9B,Y10B,Y8A,Y9A,Y10A,
     .   Z1B,Z2B,Z3B,Z4A,Z5B,Z6B,Z7B,Z8B,Z9B,Z10B,Z8A,Z9A,Z10A,
     .   VOLDP,NEL)
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
      INTEGER NEL
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
     .   W
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      DOUBLE PRECISION
     .   A4M1,B4M1,AA
      DOUBLE PRECISION
     .   D,A4MB4,A4,B4,BB,AB,B2,
     .   DRDX, DRDY, DRDZ,DET,
     .   DXDR,DXDS,DXDT,DYDR,DYDS,DYDT,DZDR,DZDS,DZDT
C-----------------------------------------------
C
      DO I=1,NEL
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
       DRDX =DYDS*DZDT-DZDS*DYDT
       DRDY =DZDS*DXDT-DXDS*DZDT
       DRDZ =DXDS*DYDT-DYDS*DXDT
C
       DET = DXDR * DRDX + DYDR * DRDY + DZDR * DRDZ
       VOLDP(I) = W * DET * ONE_OVER_6
C
      ENDDO
C
      RETURN
      END
