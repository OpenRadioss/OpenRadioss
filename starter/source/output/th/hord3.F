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
Chd|  HORD3                         source/output/th/hord3.F      
Chd|-- called by -----------
Chd|        HM_READ_THGRNE                source/output/th/hm_read_thgrne.F
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|====================================================================
        SUBROUTINE HORD3(NEL,NSEL,ISK,IASC,L)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NSEL
        INTEGER NEL(*),ISK(*),IASC(*),L
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER J2, I2, I,J, NEL1,ISK1
        INTEGER LI,LJ,K,IC1
        INTEGER :: MODE,INDICE,INDICE2
        INTEGER, DIMENSION(:), ALLOCATABLE :: ITRI,INDX
        INTEGER, DIMENSION(:), ALLOCATABLE :: IASC_SAVE,ISK_SAVE
        INTEGER, DIMENSION(70000) :: WORK
!   ---------------------------------------------
        ALLOCATE( ITRI(NSEL) )
        ALLOCATE( INDX(2*NSEL) )
        ALLOCATE( IASC_SAVE(L*NSEL) )
        ALLOCATE( ISK_SAVE(NSEL) ) 


        ITRI(1:NSEL) = NEL(1:NSEL)
        IASC_SAVE(1:L*NSEL) = IASC(1:L*NSEL)
        ISK_SAVE(1:NSEL) = ISK(1:NSEL)
        DO I=1,NSEL
            INDX(I) = I
        ENDDO
        MODE=0
        CALL MY_ORDERS( MODE, WORK, ITRI, INDX, NSEL , 1)
        
        DO I=1,NSEL
            NEL(I) = ITRI(INDX(I))
            ISK(I) = ISK_SAVE(INDX(I))
        ENDDO
        DO I=1,NSEL
            INDICE = (I-1)*L
            INDICE2 = (INDX(I)-1)*L
            DO J=1,L
                IASC(INDICE+J) = IASC_SAVE(INDICE2+J)
            ENDDO
        ENDDO

        DEALLOCATE( ITRI )
        DEALLOCATE( INDX )
        DEALLOCATE( IASC_SAVE )
        DEALLOCATE( ISK_SAVE ) 

        RETURN
        END SUBROUTINE HORD3
