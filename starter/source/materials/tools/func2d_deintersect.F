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
Chd|  FUNC2D_DEINTERSECT            source/materials/tools/func2d_deintersect.F
Chd|-- called by -----------
Chd|        LAW70_TABLE                   source/materials/mat/mat070/law70_table.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FUNC2D_DEINTERSECT(NPT, NFUNC  ,YY    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
      INTEGER ,INTENT(IN)  :: NPT
      INTEGER ,INTENT(IN)  :: NFUNC
      my_real ,DIMENSION(NPT,NFUNC) ,INTENT(INOUT):: YY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: IPT,IFUNC,I1,I2,J1,J2
      my_real :: T1,T2,Y1,Y2,DY1,DY2
      my_real ,DIMENSION(NPT) :: YTMP
      my_real ,PARAMETER :: EPS = 1.0E-1
c-----------------------------------------------
c     Check and correct segment intersections between 2 functions with same abscissas
c     with respect to the second independent variable
C=======================================================================
      YTMP(:) = ZERO   
      IF (NFUNC > 1) THEN 
        DO IFUNC = 2,NFUNC
          J1 = IFUNC - 1
          J2 = IFUNC
          YTMP(1) = YY(1,J2)
          DO IPT = 2,NPT
            I1 = IPT - 1
            I2 = IPT
c
            T1 = YY(I1,J1)
            Y1 = YY(I1,J2)
            T2 = YY(I2,J1)
            Y2 = YY(I2,J2)
            DY1 = Y1 - T1
            DY2 = Y2 - T2
            IF (DY1*DY2 < 0) THEN
              ! segment intersection, needs correction
              Y2 = T2 *(ONE + EPS)
              YY(I2,J2) = Y2
            ENDIF
            YTMP(IPT) = Y2
          END DO
          YY(1:NPT,J2) = YTMP(1:NPT)
        END DO
      END IF
c-----------
      RETURN
      END
