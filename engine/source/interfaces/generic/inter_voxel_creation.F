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
Chd|  INTER_VOXEL_CREATION          source/interfaces/generic/inter_voxel_creation.F
Chd|-- called by -----------
Chd|        INTER_PREPARE_SORT            source/interfaces/generic/inter_prepare_sort.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTER_SORTING_MOD             share/modules/inter_sorting_mod.F
Chd|====================================================================
        SUBROUTINE INTER_VOXEL_CREATION(IPARI,INTBUF_TAB,X,NIN,SORT_COMM)
!$COMMENT
!       INTER_VOXEL_CREATION description :
!
!       INTER_VOXEL_CREATION organization :
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTER_SORTING_MOD
        USE INTBUFDEF_MOD  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: NIN
        INTEGER, DIMENSION(NPARI,NINTER), INTENT(in) :: IPARI    !   interface data
        TYPE(INTBUF_STRUCT_),DIMENSION(NINTER), INTENT(in) :: INTBUF_TAB    ! interface data
        my_real, DIMENSION(3,NUMNOD), INTENT(in), TARGET :: X            !   position
        TYPE(sorting_comm_type), DIMENSION(NINTER), INTENT(inout) :: SORT_COMM   ! structure for interface sorting comm
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,J
        INTEGER :: NSN
        INTEGER :: FIRST, LAST
        my_real :: XMINB,XMAXB
        my_real :: YMINB,YMAXB
        my_real :: ZMINB,ZMAXB
!   ----------------------------------------

        XMAXB = BOX_LIMIT(1)
        YMAXB = BOX_LIMIT(2)
        ZMAXB = BOX_LIMIT(3)
        XMINB = BOX_LIMIT(4)
        YMINB = BOX_LIMIT(5)
        ZMINB = BOX_LIMIT(6)
        !   ------------------
        !   Create the secondary node voxel  : 
        NSN = IPARI(5,NIN)
        IF(NSN==0) RETURN
        !   ------------------
        !   allocation
        IF( ALLOCATED( SORT_COMM(NIN)%NEXT_NOD ) ) DEALLOCATE(SORT_COMM(NIN)%NEXT_NOD)
        IF( ALLOCATED( SORT_COMM(NIN)%LAST_NOD ) ) DEALLOCATE(SORT_COMM(NIN)%LAST_NOD)

        IF( ALLOCATED( SORT_COMM(NIN)%IIX ) ) DEALLOCATE(SORT_COMM(NIN)%IIX)
        IF( ALLOCATED( SORT_COMM(NIN)%IIY ) ) DEALLOCATE(SORT_COMM(NIN)%IIY)
        IF( ALLOCATED( SORT_COMM(NIN)%IIZ ) ) DEALLOCATE(SORT_COMM(NIN)%IIZ)
        IF( ALLOCATED( SORT_COMM(NIN)%VOXEL ) ) DEALLOCATE(SORT_COMM(NIN)%VOXEL)

        ALLOCATE(SORT_COMM(NIN)%NEXT_NOD(NSN))
        ALLOCATE(SORT_COMM(NIN)%LAST_NOD(NSN))
        ALLOCATE(SORT_COMM(NIN)%IIX(NSN))
        ALLOCATE(SORT_COMM(NIN)%IIY(NSN))
        ALLOCATE(SORT_COMM(NIN)%IIZ(NSN))

        ALLOCATE(SORT_COMM(NIN)%VOXEL(NB_CELL_X,NB_CELL_Y,NB_CELL_Z) )
        SORT_COMM(NIN)%VOXEL(1:NB_CELL_X,1:NB_CELL_Y,1:NB_CELL_Z) = 0
        !   ------------------

        !   ------------------
        !   loop over the secondary nodes
        FIRST = 0
        LAST = 0
        DO I=1,NSN                
            SORT_COMM(NIN)%IIX(I)=0                 
            SORT_COMM(NIN)%IIY(I)=0                 
            SORT_COMM(NIN)%IIZ(I)=0                 
            IF(INTBUF_TAB(NIN)%STFNS(I)==ZERO)CYCLE 
            J=INTBUF_TAB(NIN)%NSV(I)    !   id of the node (pointer to X position)
            !   ------------------        
            !   computation of the index iix/y/z   

            SORT_COMM(NIN)%IIX(I)=INT(NB_CELL_X*(X(1,J)-XMINB)/(XMAXB-XMINB))                         
            SORT_COMM(NIN)%IIY(I)=INT(NB_CELL_Y*(X(2,J)-YMINB)/(YMAXB-YMINB))                         
            SORT_COMM(NIN)%IIZ(I)=INT(NB_CELL_Z*(X(3,J)-ZMINB)/(ZMAXB-ZMINB))                         
            SORT_COMM(NIN)%IIX(I)=MAX(1,MIN(NB_CELL_X,SORT_COMM(NIN)%IIX(I)))
            SORT_COMM(NIN)%IIY(I)=MAX(1,MIN(NB_CELL_Y,SORT_COMM(NIN)%IIY(I)))
            SORT_COMM(NIN)%IIZ(I)=MAX(1,MIN(NB_CELL_Z,SORT_COMM(NIN)%IIZ(I))) 
            !   ------------------
            FIRST = SORT_COMM(NIN)%VOXEL( SORT_COMM(NIN)%IIX(I),
     .                                    SORT_COMM(NIN)%IIY(I),
     .                                    SORT_COMM(NIN)%IIZ(I) )
            !   ------------------
            IF(FIRST == 0)THEN       
                !   the cell iix/iiy/iiz is empty  
                SORT_COMM(NIN)%VOXEL( SORT_COMM(NIN)%IIX(I),
     .                                SORT_COMM(NIN)%IIY(I),
     .                                SORT_COMM(NIN)%IIZ(I)) = I ! first node of the cell
                SORT_COMM(NIN)%NEXT_NOD(I) = 0 ! there is no next node after I node                   
                SORT_COMM(NIN)%LAST_NOD(I) = 0 ! there is no last node after I node             
            ELSEIF(SORT_COMM(NIN)%LAST_NOD(FIRST) == 0)THEN                                     
                !   the cell has only one node, add the I node                             
                SORT_COMM(NIN)%NEXT_NOD(FIRST) = I ! I node is the next node                                
                SORT_COMM(NIN)%LAST_NOD(FIRST) = I ! I node is the last node                                        
                SORT_COMM(NIN)%NEXT_NOD(I)     = 0 ! there is no last node after I node                   
            ELSE                     
                !   the cell has several nodes                                
                !   need to add the I node to the last position
                LAST = SORT_COMM(NIN)%LAST_NOD(FIRST) ! last position of the cell           
                SORT_COMM(NIN)%NEXT_NOD(LAST)  = I ! I node is the next node    
                SORT_COMM(NIN)%LAST_NOD(FIRST) = I ! I node is the last node                                   
                SORT_COMM(NIN)%NEXT_NOD(I)     = 0 ! there is next node after I node           
            ENDIF 
            !   ------------------                   
        ENDDO      
        !   ------------------

        RETURN
        END SUBROUTINE INTER_VOXEL_CREATION
