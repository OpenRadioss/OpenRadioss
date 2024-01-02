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
Chd|  TABLE_VALUES_2D               source/materials/tools/table_values_2d.F
Chd|-- called by -----------
Chd|        LAW70_TABLE                   source/materials/mat/mat070/law70_table.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE TABLE_VALUES_2D(LEN,NPTF,XI,YI,XF,YF)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
c   use common abscissa vector to calculate new interpolated function values
c   NPTF >= LEN
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: LEN                    ! length of input  X vector
      INTEGER ,INTENT(IN) :: NPTF                   ! length of output X vector
      my_real ,DIMENSION(LEN)  ,INTENT(IN)  :: XI   ! initial X coordinates 
      my_real ,DIMENSION(LEN)  ,INTENT(IN)  :: YI   ! initial Y values
      my_real ,DIMENSION(NPTF) ,INTENT(IN)  :: XF   ! output  X coordinates 
      my_real ,DIMENSION(NPTF) ,INTENT(OUT) :: YF   ! output function values
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: IPT,IDX
      my_real :: X1,X2,Y1,Y2,DERI
c=======================================================================
      IDX = 1
      X1  = XI(IDX)
      Y1  = YI(IDX)
      X2  = XI(IDX+1)
      Y2  = YI(IDX+1)
      DERI  = (Y2 - Y1) / (X2 - X1)
      DO IPT = 1,NPTF
        IF (XF(IPT) >= X2 .and. IDX+1 < LEN) THEN
          IDX = IDX + 1
          X1 = X2
          Y1 = Y2
          X2 = XI(IDX+1)
          Y2 = YI(IDX+1)
          DERI  = (Y2 - Y1) / (X2 - X1)
        END IF
        YF(IPT) = Y1 + DERI * (XF(IPT) - X1)
      END DO
c-----------
      RETURN
      END SUBROUTINE TABLE_VALUES_2D
