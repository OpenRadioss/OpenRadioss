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
Chd|  A3INVDP_V                     source/elements/shell/coquez/a3invdp_v.F
Chd|-- called by -----------
Chd|        CZCORP5                       source/elements/shell/coquez/czcorp5.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE A3INVDP_V(D,DI,NEL,PLAT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"

#include "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: NEL !< number of element of the group
        LOGICAL, DIMENSION(NEL), INTENT(INOUT) :: PLAT
        my_real, DIMENSION(MVSIZ,6), INTENT(IN) :: D(MVSIZ,6)
        my_real, DIMENSION(MVSIZ,6), INTENT(INOUT) :: DI(MVSIZ,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I
        DOUBLE PRECISION :: ABC,XXYZ2,YYXZ2,ZZXY2,DETA
C-----------------------------------------------
        DO I=1,NEL
            IF(.NOT.PLAT(I)) THEN
                ABC = D(I,1)*D(I,2)*D(I,3)
                XXYZ2 = D(I,1)*D(I,6)*D(I,6)
                YYXZ2 = D(I,2)*D(I,5)*D(I,5)
                ZZXY2 = D(I,3)*D(I,4)*D(I,4)
                DETA = ABS(ABC+TWO*D(I,4)*D(I,5)*D(I,6)-XXYZ2-YYXZ2-ZZXY2)
                DETA = ONE/MAX(DETA,EM20)
                DI(I,1) = (ABC-XXYZ2)*DETA/MAX(D(I,1),EM20)
                DI(I,2) = (ABC-YYXZ2)*DETA/MAX(D(I,2),EM20)
                DI(I,3) = (ABC-ZZXY2)*DETA/MAX(D(I,3),EM20)
                DI(I,4) = (D(I,5)*D(I,6)-D(I,4)*D(I,3))*DETA
                DI(I,5) = (D(I,6)*D(I,4)-D(I,5)*D(I,2))*DETA
                DI(I,6) = (D(I,4)*D(I,5)-D(I,6)*D(I,1))*DETA
            ENDIF
        ENDDO
C
        RETURN
        END SUBROUTINE A3INVDP_V
