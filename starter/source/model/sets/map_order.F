Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  MAP_ORDER                     source/model/sets/map_order.F 
Chd|-- called by -----------
Chd|        CREATE_MAP_TABLES             source/model/sets/map_tables.F
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|====================================================================
      SUBROUTINE MAP_ORDER(IXELM,SIXELM,UID,NUM_ELM,MAP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s 
C-----------------------------------------------
        INTEGER, INTENT(in) :: NUM_ELM,SIXELM,UID
        INTEGER, DIMENSION(SIXELM,NUM_ELM), INTENT(in) :: IXELM
        INTEGER, DIMENSION(NUM_ELM,2), INTENT(inout) :: MAP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I
        INTEGER, DIMENSION(70000) :: IWORK
        INTEGER, DIMENSION(:), ALLOCATABLE :: ISORT,INDEX_SORT
C-----------------------------------------------

        ALLOCATE(ISORT(NUM_ELM))
        ALLOCATE(INDEX_SORT(2*NUM_ELM))

        DO I=1,NUM_ELM
            ISORT(I)=IXELM(UID,I)
            INDEX_SORT(I)=I
        ENDDO
        CALL MY_ORDERS(0,IWORK,ISORT,INDEX_SORT,NUM_ELM,1)

        DO I=1,NUM_ELM
            MAP(I,1)=ISORT(INDEX_SORT(I))
            MAP(I,2)=INDEX_SORT(I)
        ENDDO

        DEALLOCATE (ISORT)
        DEALLOCATE (INDEX_SORT)

        RETURN
        END SUBROUTINE MAP_ORDER
