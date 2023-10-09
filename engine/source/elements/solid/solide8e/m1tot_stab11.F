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
      SUBROUTINE M1TOT_STAB11(SIG,G2,G,NEL )
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
      my_real, DIMENSION(MVSIZ), INTENT(INOUT)  :: G
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,ipr
      my_real
     .   FF,FACG,FACC,FRHO,P,FACMAX,FACD
C-----------------------------------------------
C--- only need for Isolid=18     
        FACMAX=1.5
        DO I=1,NEL
         FF = -MIN(SIG(I,1),SIG(I,2),SIG(I,3))
         IF (FF <=G2 ) CYCLE
         FACG = MAX(ONE,SQRT(FF/G2))
         FACG = MIN(FACMAX,FACG)
         P = -THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
         SIG(I,1)=FACG*(SIG(I,1)+P)-P
         SIG(I,2)=FACG*(SIG(I,2)+P)-P
         SIG(I,3)=FACG*(SIG(I,3)+P)-P
         SIG(I,4)=FACG*SIG(I,4)
         SIG(I,5)=FACG*SIG(I,5)
         SIG(I,6)=FACG*SIG(I,6)
         G(I) = FACG*G(I)
        END DO
C      
      RETURN
      END
