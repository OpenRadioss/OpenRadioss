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
      SUBROUTINE NSVIS_STAB(SIG  ,C1  ,G2    ,VOL  ,D1   ,
     .                     D2    ,D3  ,D4    ,D5   ,D6   ,
     .                     RHOREF,G   ,NPG   ,NEL ) 
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER, INTENT(IN) ::NEL,NPG
      my_real, DIMENSION(MVSIZ),INTENT(IN) :: VOL,
     .                   D1, D2, D3,D4, D5, D6,RHOREF
      my_real, INTENT(IN) :: C1,G2
      my_real, DIMENSION(NEL,6), INTENT(INOUT) :: SIG
      my_real, DIMENSION(MVSIZ),INTENT(INOUT) :: G
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MT
C     REAL
      my_real
     .   DD, AL, CNS1, CNS2, CNS3,SSP1,MU,
     .   DAV, PVIS, NRHO,JAC,FAC,TOL,FF,RHOSSP,FACG
#ifdef MYREAL8 
      my_real, PARAMETER :: REAL_THREE = 3.0D0
      my_real, PARAMETER :: REAL_ONE = 1.0D0
#else
      my_real, PARAMETER :: REAL_THREE = 3.0
      my_real, PARAMETER :: REAL_ONE = 1.0
#endif
C-----------------------------------------------
        MU =ZEP02
        SSP1 = TWO_THIRD*G2+C1
        DO I=1,NEL
          FF = -MIN(SIG(I,1),SIG(I,2),SIG(I,3))
          IF (FF <=G2 ) CYCLE
          FACG = MAX(ONE,SQRT(FF/G2))
          AL = (NPG*VOL(I))**(REAL_ONE/REAL_THREE)
          RHOSSP = SQRT(SSP1*RHOREF(I))
          CNS2=FACG*MU*AL*RHOSSP
          CNS3=HALF*CNS2
          DD =-D1(I)-D2(I)-D3(I)
          DAV=DD * THIRD
          SIG(I,1)=SIG(I,1) + CNS2 *(D1(I)+DAV) 
          SIG(I,2)=SIG(I,2) + CNS2 *(D2(I)+DAV) 
          SIG(I,3)=SIG(I,3) + CNS2 *(D3(I)+DAV) 
          SIG(I,4)=SIG(I,4) + CNS3 * D4(I)
          SIG(I,5)=SIG(I,5) + CNS3 * D5(I)
          SIG(I,6)=SIG(I,6) + CNS3 * D6(I)
          FAC = ONE+TWO*FACG*MU 
          G(I) =FAC*G(I)   ! time step concern
        ENDDO
C      
      RETURN
      END
