Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!	------------------------------------
	SUBROUTINE UPDATE_ACC(JFT, JLT, A, FOR )

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com01_c.inc"

!	------------------------------------
        INTEGER, INTENT(IN) :: JFT,JLT
!---
C     REAL ou REAL*8
!
        my_real, DIMENSION(NUMNOD,5) :: FOR
	my_real, DIMENSION(5,NUMNOD) :: A
!	------------------------------------
        INTEGER :: I
!	------------------------------------

	DO I=JFT,JLT
        	A(1,I) = A(1,I) + FOR(I,1) 
        	A(2,I) = A(2,I) + FOR(I,2) 
        	A(3,I) = A(3,I) + FOR(I,3) 
        	A(4,I) = A(4,I) + FOR(I,4) 
        	A(5,I) = A(5,I) + FOR(I,5) 
	ENDDO

	RETURN
!	------------------------------------
	END SUBROUTINE UPDATE_ACC
!	------------------------------------
