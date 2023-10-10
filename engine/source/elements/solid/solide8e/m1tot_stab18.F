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
      SUBROUTINE M1TOT_STAB18(SIG,SIGL,G2,G,OFFG,ISMSTR,NEL )
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
      INTEGER, INTENT(IN) :: NEL,ISMSTR
C
      my_real, INTENT(IN) :: G2
      my_real, DIMENSION(NEL,6), INTENT(INOUT) :: SIG,SIGL
      my_real, DIMENSION(NEL), INTENT(IN)  :: OFFG
      my_real, DIMENSION(MVSIZ), INTENT(INOUT)  :: G
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,ipr
      my_real
     .   FF,FACG,FACC,FRHO,P,FACMAX,FACD,DP,SIG2(6),FAC
C-----------------------------------------------
        DO I=1,NEL
         FF = -1.6*MIN(SIG(I,1),SIG(I,2),SIG(I,3))
         IF (OFFG(I)>ONE.OR.FF <=G2) CYCLE
         FACG = MAX(ONE,SQRT(FF/G2)) + 0.15*FF/G2
         FACD = MAX(one,0.85*FACG)
         SIG(I,4)=FACG*SIG(I,4)
         SIG(I,5)=FACG*SIG(I,5)
         SIG(I,6)=FACG*SIG(I,6)
         P = -THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
         SIG(I,1)=FACD*(SIG(I,1)+P)-P
         SIG(I,2)=FACD*(SIG(I,2)+P)-P
         SIG(I,3)=FACD*(SIG(I,3)+P)-P
         G(I) = FACG*G(I)
        END DO
        IF (ISMSTR==12) THEN
          FACMAX = 4.5
          DO I=1,NEL
           IF (OFFG(i)<=ONE) CYCLE
           FF = -MIN(SIG(I,1),SIG(I,2),SIG(I,3))
           IF (FF <=G2 ) CYCLE
           FAC = 1.25*MAX(ONE,FF/G2)
           FACG = MIN(FACMAX,FAC-ONE)
           SIG2(1) = SIG(I,1)-SIGL(I,1)
           SIG2(2) = SIG(I,2)-SIGL(I,2)
           SIG2(3) = SIG(I,3)-SIGL(I,3)
           SIG2(4) = SIG(I,4)-SIGL(I,4)
           SIG2(5) = SIG(I,5)-SIGL(I,5)
           SIG2(6) = SIG(I,6)-SIGL(I,6)
           SIG(I,4)=SIG(I,4)+FACG*SIG2(4)
           SIG(I,5)=SIG(I,5)+FACG*SIG2(5)
           SIG(I,6)=SIG(I,6)+FACG*SIG2(6)
           P = -THIRD*(SIG2(1)+SIG2(2)+SIG2(3))
           FACD = MIN(1.2,FACG)
           SIG(I,1)=SIG(I,1)+FACD*(SIG2(1)+P)
           SIG(I,2)=SIG(I,2)+FACD*(SIG2(2)+P)
           SIG(I,3)=SIG(I,3)+FACD*(SIG2(3)+P)
           G(I) = FAC*G(I)
          END DO
        END IF !(ISMSTR==12) THEN
C      
      RETURN
      END
