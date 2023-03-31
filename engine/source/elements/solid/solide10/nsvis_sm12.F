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
Chd|  NSVIS_SM12                    source/elements/solid/solide10/nsvis_sm12.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE NSVIS_SM12(OFFG ,MU ,SSP  ,VOL  ,D1   ,
     .                      D2   ,D3  ,D4    ,D5  ,D6   ,
     .                      VOL0 ,RHO0,STI   ,NEL   ) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "nsvis_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER, INTENT(IN) ::NEL
      my_real, DIMENSION(MVSIZ),INTENT(IN) :: VOL,
     .                   D1, D2, D3,D4, D5, D6,SSP
      my_real, DIMENSION(NEL),INTENT(IN) :: OFFG,VOL0
      my_real, DIMENSION(MVSIZ),INTENT(INOUT) :: STI
      my_real, INTENT(IN) :: RHO0,MU
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MT
C     REAL
      my_real
     .   DD, AL, CNS1, CNS2, CNS3,
     .   DAV, PVIS, NRHO,JAC,FAC,TOL
#ifdef MYREAL8 
      my_real, PARAMETER :: REAL_THREE = 3.0D0
      my_real, PARAMETER :: REAL_ONE = 1.0D0
#else
      my_real, PARAMETER :: REAL_THREE = 3.0
      my_real, PARAMETER :: REAL_ONE = 1.0
#endif
C-----------------------------------------------
        TOL = ONE-EM02
      IF (MU>ZERO) THEN
        DO I=1,NEL
          JAC = VOL(I)/VOL0(I)
          IF (OFFG(I)<=ONE.OR.JAC>TOL) CYCLE
          AL = VOL(I)**(REAL_ONE/REAL_THREE)
          NRHO = RHO0/JAC
          CNS2=MU*AL*NRHO*SSP(I)
          CNS3=HALF*CNS2
          DD =-D1(I)-D2(I)-D3(I)
          DAV=DD * THIRD
          SVIS(I,1)=SVIS(I,1) + CNS2 *(D1(I)+DAV) 
          SVIS(I,2)=SVIS(I,2) + CNS2 *(D2(I)+DAV) 
          SVIS(I,3)=SVIS(I,3) + CNS2 *(D3(I)+DAV) 
          SVIS(I,4)=SVIS(I,4) + CNS3 * D4(I)
          SVIS(I,5)=SVIS(I,5) + CNS3 * D5(I)
          SVIS(I,6)=SVIS(I,6) + CNS3 * D6(I)
        ENDDO
      END IF !(MU>ZERO) THEN
C      
      RETURN
      END
