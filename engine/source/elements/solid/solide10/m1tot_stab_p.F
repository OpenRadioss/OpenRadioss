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
Chd|  M1TOT_STAB_P                  source/elements/solid/solide10/m1tot_stab_p.F
Chd|-- called by -----------
Chd|        M1LAWTOT                      source/materials/mat/mat001/m1lawtot.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M1TOT_STAB_P(SIG,G2,C1,NEL )
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
      INTEGER, INTENT(IN) :: NEL
C
      my_real, INTENT(IN) :: G2
      my_real, DIMENSION(NEL,6), INTENT(INOUT) :: SIG
      my_real, DIMENSION(MVSIZ), INTENT(INOUT)  :: C1
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,ISTAB
      my_real
     .   FF,FACG,FRHO,P,FACMAX,FACD,PHYD,FAC,DP
C-----------------------------------------------
        DO I=1,NEL
         FF = -MIN(SIG(I,1),SIG(I,2),SIG(I,3))
         IF (FF <=G2 ) CYCLE
         FACD = 1.2*SQRT(FF/G2)
         P = THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
         DP = (FACD-ONE)*P
         SIG(I,1)=SIG(I,1)+DP
         SIG(I,2)=SIG(I,2)+DP
         SIG(I,3)=SIG(I,3)+DP
         C1(I) = FACD*C1(I)
        END DO
C      
      RETURN
      END
