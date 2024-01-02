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
C
Chd|====================================================================
Chd|  HORD                          source/output/th/hord.F       
Chd|-- called by -----------
Chd|        HM_READ_THGRKI                source/output/th/hm_read_thgrki.F
Chd|        HM_READ_THGRKI_RBODY          source/output/th/hm_read_thgrki_rbody.F
Chd|        HM_READ_THGRSENS              source/output/th/hm_read_thgrsens.F
Chd|        HM_READ_THGRSURF              source/output/th/hm_read_thgrsurf.F
Chd|        HM_THGRKI_VENT                source/output/th/hm_thgrki_vent.F
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|====================================================================
        SUBROUTINE HORD(NEL,NSEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NSEL
        INTEGER NEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: J2, I2, I, NEL1, MODE
        INTEGER, DIMENSION(:), ALLOCATABLE :: ITRI,INDX
        INTEGER, DIMENSION(70000) :: WORK
!   ---------------------------------------------
        ALLOCATE( ITRI(NSEL) )
        ALLOCATE( INDX(2*NSEL) )

        ITRI(1:NSEL) = NEL(1:NSEL)
        DO I=1,NSEL
            INDX(I) = I
        ENDDO
        MODE=0
        CALL MY_ORDERS( MODE, WORK, ITRI, INDX, NSEL , 1)
        
        DO I=1,NSEL
            NEL(I) = ITRI(INDX(I))
        ENDDO

        DEALLOCATE( ITRI )
        DEALLOCATE( INDX )

        RETURN
        END SUBROUTINE HORD
