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
	SUBROUTINE UPDATE_FOR(JFT  ,JLT   ,NEL   ,SIGO   ,FOR , OFF)

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"

!	------------------------------------
        INTEGER, INTENT(IN) :: JFT,JLT,NEL
!---
C     REAL ou REAL*8
!
	my_real, DIMENSION(NEL) :: OFF
        my_real, DIMENSION(NEL,5) :: SIGO,FOR
!	------------------------------------
        INTEGER :: I
!	------------------------------------

       	FOR(JFT:JLT,1) = FOR(JFT:JLT,1) + SIGO(JFT:JLT,1)*UNDEMI
       	FOR(JFT:JLT,2) = FOR(JFT:JLT,2) + SIGO(JFT:JLT,2)*UNDEMI
       	FOR(JFT:JLT,3) = FOR(JFT:JLT,3) + SIGO(JFT:JLT,3)*UNDEMI
       	FOR(JFT:JLT,4) = FOR(JFT:JLT,4) + SIGO(JFT:JLT,4)*UNDEMI
       	FOR(JFT:JLT,5) = FOR(JFT:JLT,5) + SIGO(JFT:JLT,5)*UNDEMI

!	if the element is deactivated --> for = 0
        FOR(JFT:JLT,1) = FOR(JFT:JLT,1) * OFF(JFT:JLT)
        FOR(JFT:JLT,2) = FOR(JFT:JLT,2) * OFF(JFT:JLT)
        FOR(JFT:JLT,3) = FOR(JFT:JLT,3) * OFF(JFT:JLT)
        FOR(JFT:JLT,4) = FOR(JFT:JLT,4) * OFF(JFT:JLT)
        FOR(JFT:JLT,5) = FOR(JFT:JLT,5) * OFF(JFT:JLT)

	RETURN
!	------------------------------------
	END SUBROUTINE UPDATE_FOR
!	------------------------------------
