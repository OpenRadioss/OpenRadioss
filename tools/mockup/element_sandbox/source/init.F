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
	SUBROUTINE INIT(NBR_GROUP,IGROUP,NUMNOD,NGL)

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
	
!	------------------------------------
        INTEGER, INTENT(IN) :: NBR_GROUP,NUMNOD
        INTEGER, DIMENSION(NUMNOD), INTENT(OUT) :: NGL
        INTEGER, DIMENSION(11,NBR_GROUP), INTENT(OUT) :: IGROUP
!	------------------------------------
        INTEGER :: I,NG,ADFGR
        INTEGER :: JFT,JLT,IPLA,NEL,JTHE
        INTEGER :: IPT,NPTTOT,IMAT,ISRATE,MLW
!	------------------------------------
        DO NG=1,NBR_GROUP
                IGROUP(1,NG) = MVSIZ-1  !       number of element per group
                IGROUP(2,NG) = 1 + mod(ng,2)        !       material law
                IGROUP(3,NG) = 1        !       first element of the group
                IGROUP(4,NG) = IGROUP(1,NG)!       last element of the group
                IGROUP(6,NG) = 0        !       plasticity
                IGROUP(7,NG) = 0        !       ???
                IGROUP(8,NG) = 1        !       current integration point
                IGROUP(9,NG) = 5        !       total number of integration point
                IGROUP(10,NG) = 0       !       ???
                IGROUP(11,NG) = modulo(ng,2)       !       ???
        ENDDO
        IGROUP(5,1) = 0
        DO NG=2,NBR_GROUP
                IGROUP(5,NG) = IGROUP(5,NG-1)+IGROUP(1,NG-1)
        ENDDO
        DO I=1,NUMNOD
                NGL(I) = I
        ENDDO

        RETURN
	
!	------------------------------------
	END SUBROUTINE INIT
!	------------------------------------
