Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    roto_tens2d_aniso   ../engine/source/materials/tools/roto_tens2d_aniso.F
!||--- called by ------------------------------------------------------
!||    h3d_shell_tensor    ../engine/source/output/h3d/h3d_results/h3d_shell_tensor.F
!||====================================================================
      SUBROUTINE ROTO_TENS2D_ANISO(NEL,TENS,DIR_A,DIR_B)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL
      my_real ,DIMENSION(NEL,2) ,INTENT(IN)    :: DIR_A,DIR_B
      my_real ,DIMENSION(NEL,3) ,INTENT(INOUT) :: TENS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I
      my_real :: R1,R2,S1,S2,T1,T2,T3
c=======================================================================
      DO I = 1,NEL
        T1 = TENS(I,1)
        T2 = TENS(I,2)
        T3 = TENS(I,3)
        R1 = DIR_A(I,1)
        S1 = DIR_A(I,2)
        R2 = DIR_B(I,1)
        S2 = DIR_B(I,2)
        TENS(I,1) = R1*R1*T1 + S1*S1*T2 + TWO*R1*S1*T3        ! eps_x dir1
        TENS(I,2) = R2*R2*T1 + S2*S2*T2 + TWO*R2*S2*T3        ! eps_y dir2           
        TENS(I,3) = ATAN((R1*R2 + S1*S2) / (R1*S2 - R2*S1))   ! angle xy
      ENDDO
c-----------
      RETURN
      END
