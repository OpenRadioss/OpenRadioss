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
Chd|  SSORT_N4                      source/elements/solid/solide/ssort_n4.F
Chd|-- called by -----------
Chd|        SFOR_N2S4                     source/elements/solid/solide/sfor_n2s4.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SSORT_N4(XI,      YI,     ZI ,   MARGE,    
     .                     X1,     X2,      X3,      X4,
     .                     Y1,     Y2,      Y3,      Y4,
     .                     Z1,     Z2,      Z3,      Z4,
     .                   IFC1,   STIF,     NEL)
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
      INTEGER, INTENT (IN)  :: NEL
      INTEGER, DIMENSION(MVSIZ),INTENT (INOUT) :: IFC1
      my_real, DIMENSION(MVSIZ), INTENT (IN) :: MARGE,
     .                        XI,     YI,     ZI,STIF 
      DOUBLE PRECISION, DIMENSION(MVSIZ), INTENT (IN) ::
     .                        X1,     X2,      X3,      X4,
     .                        Y1,     Y2,      Y3,      Y4,
     .                        Z1,     Z2,      Z3,      Z4
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
C                                                                     12
      my_real
     .   RX, RY, RZ, SX, SY, SZ,NX,NY,NZ,BBB,
     .   DX,DY,DZ,DD,PENE(MVSIZ),NORM,DMIN
C----------------------------
         DO I=1,NEL
           IF (IFC1(I)>0) CYCLE
           RX =X2(I)+X3(I)-X1(I)-X4(I)
           RY =Y2(I)+Y3(I)-Y1(I)-Y4(I)
           RZ =Z2(I)+Z3(I)-Z1(I)-Z4(I)
           SX =X3(I)+X4(I)-X1(I)-X2(I)
           SY =Y3(I)+Y4(I)-Y1(I)-Y2(I)
           SZ =Z3(I)+Z4(I)-Z1(I)-Z2(I)
           NX =RY*SZ - RZ*SY
           NY =RZ*SX - RX*SZ
           NZ =RX*SY - RY*SX
           NORM=ONE/MAX(EM20,SQRT(NX*NX+NY*NY+NZ*NZ))
           BBB = ((X3(I)-XI(I))*NX + (Y3(I)-YI(I))*NY +
     .            (Z3(I)-ZI(I))*NZ)*NORM 
           PENE(I) = ABS(BBB)  
           IF (PENE(I)<MARGE(I).AND.STIF(I)>ZERO) IFC1(I)=2
         ENDDO
C-------if degenerated quad not possible have 1=3 or 2=4
#include "vectorize.inc"
         DO I=1,NEL
           IF (IFC1(I)==0) CYCLE
           DX =X4(I)-X3(I)
           DY =Y4(I)-Y3(I)
           DZ =Z4(I)-Z3(I)
           DMIN = ABS(DX)+ABS(DY)+ABS(DZ)
           IF (DMIN==ZERO) THEN
             IFC1(I)=3
             CYCLE
           END IF
           DX =X2(I)-X1(I)
           DY =Y2(I)-Y1(I)
           DZ =Z2(I)-Z1(I)
           DMIN = ABS(DX)+ABS(DY)+ABS(DZ)
           IF (DMIN==ZERO) THEN
             IFC1(I)=6
             CYCLE
           END IF
           DX =X4(I)-X1(I)
           DY =Y4(I)-Y1(I)
           DZ =Z4(I)-Z1(I)
           DMIN = ABS(DX)+ABS(DY)+ABS(DZ)
           IF (DMIN==ZERO) THEN
             IFC1(I)=4
             CYCLE
           END IF
           DX =X3(I)-X2(I)
           DY =Y3(I)-Y2(I)
           DZ =Z3(I)-Z2(I)
           DMIN = ABS(DX)+ABS(DY)+ABS(DZ)
           IF (DMIN==ZERO) THEN
             IFC1(I)=5
             CYCLE
           END IF
         ENDDO
C       
      RETURN
      END
